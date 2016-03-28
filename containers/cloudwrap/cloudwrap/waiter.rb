#!/usr/bin/env ruby

require 'net/ssh'
require 'net/scp'
require 'fog'
require 'json'
require 'rest-client'
require 'diplomat'
require 'base64'
require 'tempfile'
require './common'
# openstack uses the python API via CLI commands
require './openstack'  

loop do
  begin
    log "Finding endpoints of servers to check"
    device_data = {}
    endpoints = {}
    servers = {}
    response = RestClient.get('http://localhost:8500/v1/kv/cloudwrap/create', params: {recurse: true}) rescue nil
    JSON.parse(response.body).each do |k|
      ep = JSON.parse(Base64.decode64(k["Value"]))
      endpoints[ep] ||= get_endpoint(ep)
      cloudwrap_device_id = k["Key"].split("/",4)[-1]
      rebar_id = k["Key"].split("/",4)[-2]

      case ep['provider']
      when 'AWS', 'Google'
        servers[k["Key"]] = [rebar_id, endpoints[ep].servers.get(cloudwrap_device_id), endpoints[ep], ep]
      when 'OpenStack'
        servers[k["Key"]] = [rebar_id, cloudwrap_device_id, nil, ep]
      when 'Packet'
        servers[k["Key"]] = [rebar_id, cloudwrap_device_id, nil, ep]
      end
    end if response && response.code == 200
    servers.each do |key, val|
      rebar_id = val[0]
      cloudwrap_device_id = server = val[1]
      ep = val[2]
      endpoint = val[3]

      # Load key
      kp_name = keyfile_name(rebar_id)
      kp_loc = File.expand_path("~/.ssh/#{kp_name}")
      if ! File.exists?(kp_loc)
        ssh_key=Diplomat::Kv.get("cloudwrap/keys/#{rebar_id}")
        File.open(kp_loc,"w") do |f|
          f.write(ssh_key)
          f.flush
          f.chmod(0600)
        end
      end

      # State good?
      case endpoint['provider']
      when 'AWS', 'Google'
        log "` #{server.id}"
        unless server.ready?
          log "Server #{server.id} not ready, skipping"
          next
        end
      when 'OpenStack'
        log "Testing server #{rebar_id} as OpenStack #{cloudwrap_device_id} state"
        device_data = OpenStack::get(endpoint, cloudwrap_device_id)
        unless device_data['status'] == 'ACTIVE'
          log("Server OpenStack #{cloudwrap_device_id} is #{device_data['status']}, skipping")
          if device_data["status"] == nil
            log("Looking for #{cloudwrap_device_id}, but was empty - removing")
            Diplomat::Kv.delete(key)
          end
          next
        end
      when 'Packet'
        log "Testing server #{rebar_id} #{cloudwrap_device_id} state"
        response = nil
        begin
          response = RestClient.get "https://api.packet.net/projects/#{endpoint['project_id']}/devices/#{cloudwrap_device_id}", content_type: :json, accept: :json, 'X-Auth-Token' => endpoint['project_token']
        rescue RestClient::ResourceNotFound => e2
          log("Looking for #{cloudwrap_device_id}, but was delete - removing")
          Diplomat::Kv.delete(key)
        rescue Exception => e
          log("Looking for #{cloudwrap_device_id}, but got #{e.inspect}")
        end
        next if !response || response.code != 200

        device_data = JSON.parse(response.body)

        unless device_data['state'] == 'active'
          log("Server #{cloudwrap_device_id} is #{device_data['state']}, skipping")
          next
        end
      end

      # SSH able?
      case endpoint['provider']
      when 'AWS', 'Google'
        server.private_key_path = kp_loc
        unless %w(rebar ec2-user ubuntu centos root).find do |user|
                 server.username = user
                 server.sshable? rescue false
               end
          log "Server #{server.id} not sshable, skipping"
          next
        end

        dev_ip = server.public_ip_address
        cidr = "32"

        private_dev_ip = server.private_ip_address
        private_dev_cidr = "32"

      when 'OpenStack'

        dev_ip = OpenStack::public_v4(device_data['addresses'])
        cidr = "32"
        username = nil
        users = (endpoint['os-ssh-user'] || "ubuntu centos root").split(/\s|,/)

        log "Make sure node #{dev_ip} is ssh-able? (user #{users}@#{device_data['addresses']})"
        users.each do |user|
          begin
            Net::SSH.start(dev_ip, user, { keys: [ kp_loc ], paranoid: false }) do |ssh|              
              ssh.exec!("date") # capture all stderr and stdout output from a remote process
            end
            username = user
            log "Server user #{username} on #{cloudwrap_device_id} works.  Proceeding..."
            break
          rescue Exception => e
            log "Server user #{user} on #{cloudwrap_device_id} not sshable, skipping: #{e}"
          end
        end

        # MISSING!
        private_dev_ip = ""
        private_dev_cidr = "32"

      when 'Packet'
        # For now, make packet private and public the same.
        # The node can bind to either.
        username = 'root'
        cidr = device_data['ip_addresses'][0]['cidr']
        dev_ip = device_data['ip_addresses'][0]['address']
        private_dev_ip = device_data['ip_addresses'][0]['address']
        private_dev_cidr = device_data['ip_addresses'][0]['cidr']

        log "Make sure node is ssh-able?"
        begin
          Net::SSH.start(dev_ip, 'root', { keys: [ kp_loc ], paranoid: false }) do |ssh|
            # capture all stderr and stdout output from a remote process
            ssh.exec!("date")
          end
        rescue Exception => e
          log "Server #{cloudwrap_device_id} not sshable, skipping: #{e}"
          next
        end
      end

      log "Adding rebar keys and enabling SSH in as root"
      case endpoint['provider']
      when 'AWS', 'Google'
        server.ssh("sudo -- mkdir -p /root/.ssh")
        server.ssh("sudo -- sed -i -r '/(PasswordAuthentication|PermitRootLogin)/d' /etc/ssh/sshd_config")
        server.ssh("sudo -- printf '\nPasswordAuthentication %s\nPermitRootLogin %s\n' no yes >> /etc/ssh/sshd_config")
        server.ssh("sudo -- service ssh restart")
        server.ssh("sudo -- service sshd restart")
        Tempfile.open("cloudwrap-keys") do |f|
          pubkeys = JSON.parse(`rebar deployments get system attrib rebar-access_keys`)
          pubkeys['value'].each_value do |v|
            f.puts(v.strip)
          end
          f.flush
          f.fsync
          server.scp(f.path,"/tmp/rebar_keys")
        end
        server.ssh("sudo -- mv /tmp/rebar_keys /root/.ssh/authorized_keys")
        server.ssh("sudo -- chmod 600 /root/.ssh/authorized_keys")
        server.ssh("sudo -- chown root:root /root/.ssh/authorized_keys")

        answer = server.ssh("sudo -- ip addr | grep #{private_dev_ip} | awk '{ print $2 }' | awk -F/ '{ print $2 }'")
        private_dev_cidr = answer[0].stdout.strip

      when 'Packet', 'OpenStack'
        log "Put keys to node"
        Tempfile.open("cloudwrap-keys") do |f|
          pubkeys = JSON.parse(`rebar deployments get system attrib rebar-access_keys`)
          pubkeys['value'].each_value do |v|
            f.puts(v.strip)
          end
          f.flush
          f.fsync
          begin
            Net::SCP.upload!(dev_ip, username, f.path, "/tmp/rebar_keys", { ssh: { keys: [ kp_loc ], paranoid: false } })
          rescue Exception => e
            log "Server #{cloudwrap_device_id} failed to upload keys, skipping: #{e}"
            next
          end
        end

        log "Put keys in place on node"
        begin
          Net::SSH.start(dev_ip, username, { keys: [ kp_loc ], paranoid: false }) do |ssh|
            log "SSH keysetup using `ssh #{username}@#{dev_ip} -i #{kp_loc}`"
            # capture all stderr and stdout output from a remote process
            if username!='root' && endpoint['provider']=='OpenStack'
              # OpenStack accounts are not always root, add root
              log "Adding root account for #{endpoint['provider']} from #{username}@#{dev_ip} account"
              ssh.exec!("sudo -- mkdir -p /root/.ssh")
              ssh.exec!("sudo -- sed -i -r '/(PasswordAuthentication|PermitRootLogin)/d' /etc/ssh/sshd_config")
              ssh.exec!("sudo -- printf '\nPasswordAuthentication %s\nPermitRootLogin %s\n' no yes >> /etc/ssh/sshd_config")
              ssh.exec!("sudo -- service ssh restart")
              ssh.exec!("sudo -- service sshd restart")
              ssh.exec!("sudo -- mv /tmp/rebar_keys /root/.ssh/authorized_keys")
              ssh.exec!("sudo -- chmod 600 /root/.ssh/authorized_keys")
              ssh.exec!("sudo -- chown root:root /root/.ssh/authorized_keys")
            else
              log "Root account exists, just add the keys"
              ssh.exec!("cat /tmp/rebar_keys >> /root/.ssh/authorized_keys")
              ssh.exec!("rm -f /tmp/rebar_keys")
            end
          end
        rescue Exception => e
          log "Server #{cloudwrap_device_id} not key updatable, skipping: #{e}"
          next
        end
      end

      log("Adding node control address #{dev_ip}/#{cidr} to node #{rebar_id}")
      system("rebar nodes set #{rebar_id} attrib node-control-address to '{\"value\": \"#{dev_ip}/#{cidr}\"}'")
      log("Adding node private control address #{private_dev_ip}/#{private_dev_cidr} to node #{rebar_id}")
      system("rebar nodes set #{rebar_id} attrib node-private-control-address to '{\"value\": \"#{private_dev_ip}/#{private_dev_cidr}\"}'")
      log "Marking server #{rebar_id} alive"
      Diplomat::Kv.delete(key)
      system("rebar nodes update #{rebar_id} '{\"alive\": true, \"available\": true}'")
      Diplomat::Kv.delete("cloudwrap/keys/#{rebar_id}")
      if ep && ep.respond_to?(:key_pairs)
        old_kp = ep.key_pairs.get(kp_name)
        old_kp.destroy if old_kp
      elsif ep.nil? # Packet or OpenStack

        case endpoint['provider']
        when "OpenStack"
          # OpenStack::deletekey endpoint, kp_name 
        when 'Packet'
          # Packet endpoint has the account key
          packet_project_token = endpoint['project_token']

          found, wrong, key_id = get_packet_key_info(packet_project_token, kp_name, '')
          if key_id
            response = nil
            begin
              response = RestClient.delete "https://api.packet.net/ssh-keys/#{key_id}",
                                           content_type: :json, accept: :json,
                                           'X-Auth-Token' => packet_project_token
            rescue Exception => e
              log("Failed to remove key: #{e.inspect}")
              return e.inspect
            end

            if response.code != 204
              log("Failed to delete device for #{endpoint} #{id}")
              raise "Failed to delete device for #{endpoint} #{id}"
            end
          end
        end
      end
      File::delete(kp_loc)
    end
  rescue Exception => e
    log "Caught error, looping"
    log "Exception: #{e.message}"
    log e.backtrace
  end
  sleep 10
end
