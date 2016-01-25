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

loop do
  begin
    log "Finding endpoints of servers to check"
    endpoints = {}
    servers = {}
    response = RestClient.get('http://localhost:8500/v1/kv/cloudwrap/create', params: {recurse: true}) rescue nil
    JSON.parse(response.body).each do |k|
      ep = JSON.parse(Base64.decode64(k["Value"]))
      endpoints[ep] ||= get_endpoint(ep)
      packet_device_id = k["Key"].split("/",4)[-1]
      rebar_id = k["Key"].split("/",4)[-2]

      case ep['provider']
      when 'AWS', 'Google'
        servers[k["Key"]] = [rebar_id, endpoints[ep].servers.get(packet_device_id), endpoints[ep], ep]
      when 'Packet'
        servers[k["Key"]] = [rebar_id, packet_device_id, nil, ep]
      end
    end if response && response.code == 200
    servers.each do |key, val|
      rebar_id = val[0]
      packet_device_id = server = val[1]
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
        log "Testing server #{server.id}"
        unless server.ready?
          log "Server #{server.id} not ready, skipping"
          next
        end
      when 'Packet'
        log "Testing server #{rebar_id} #{packet_device_id} state"
        response = nil
        begin
          response = RestClient.get "https://api.packet.net/projects/#{endpoint['project_id']}/devices/#{packet_device_id}", content_type: :json, accept: :json, 'X-Auth-Token' => endpoint['project_token']
        rescue RestClient::ResourceNotFound => e2
          log("Looking for #{packet_device_id}, but was delete - removing")
          Diplomat::Kv.delete(key)
        rescue Exception => e
          log("Looking for #{packet_device_id}, but got #{e.inspect}")
        end
        next if !response || response.code != 200

        device_data = JSON.parse(response.body)

        unless device_data['state'] == 'active'
          log("Server #{packet_device_id} is #{device_data['state']}, skipping")
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
        cidr = "#{dev_ip}/32"

      when 'Packet'
        cidr = device_data['ip_addresses'][0]['cidr']
        dev_ip = device_data['ip_addresses'][0]['address']

        log "Make sure node is ssh-able?"
        begin
          Net::SSH.start(dev_ip, 'root', { keys: [ kp_loc ], paranoid: false }) do |ssh|
            # capture all stderr and stdout output from a remote process
            ssh.exec!("date")
          end
        rescue Exception => e
          log "Server #{packet_device_id} not sshable, skipping: #{e}"
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
      when 'Packet'
        log "Put keys to node"
        Tempfile.open("cloudwrap-keys") do |f|
          pubkeys = JSON.parse(`rebar deployments get system attrib rebar-access_keys`)
          pubkeys['value'].each_value do |v|
            f.puts(v.strip)
          end
          f.flush
          f.fsync
          begin
            Net::SCP.upload!(dev_ip, 'root', f.path, "/tmp/rebar_keys", { keys: [ kp_loc ], paranoid: false })
          rescue Exception => e
            log "Server #{packet_device_id} failed to upload keys, skipping: #{e}"
            next
          end
        end

        log "Put keys in place on node"
        begin
          Net::SSH.start(dev_ip, 'root', { keys: [ kp_loc ], paranoid: false }) do |ssh|
            # capture all stderr and stdout output from a remote process
            ssh.exec!("cat /tmp/rebar_keys >> /root/.ssh/authorized_keys")
            ssh.exec!("rm -f /tmp/rebar_keys")
          end
        rescue Exception => e
          log "Server #{packet_device_id} not key updatable, skipping: #{e}"
          next
        end
      end

      log("Adding node control address #{cidr} to node #{rebar_id}")
      system("rebar nodes set #{rebar_id} attrib node-control-address to '{\"value\": \"#{cidr}\"}'")
      log "Marking server #{rebar_id} alive"
      Diplomat::Kv.delete(key)
      Diplomat::Kv.delete("cloudwrap/keys/#{rebar_id}")
      system("rebar nodes update #{rebar_id} '{\"alive\": true, \"available\": true}'")
      if ep && ep.respond_to?(:key_pairs)
        old_kp = ep.key_pairs.get(kp_name)
        old_kp.destroy if old_kp
      else if ep.nil? # Packet
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
            return e.inspect
          end

          if response.code != 204
            raise "Failed to delete device for #{endpoint} #{id}"
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
