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
    response = RestClient.get('http://localhost:8500/v1/kv/fogwrap/create', params: {recurse: true}) rescue nil
    JSON.parse(response.body).each do |k|
      ep = JSON.parse(Base64.decode64(k["Value"]))
      endpoints[ep] ||= get_endpoint(ep)
      fog_id = k["Key"].split("/",4)[-1]
      rebar_id = k["Key"].split("/",4)[-2]
      servers[k["Key"]] = [rebar_id, endpoints[ep].servers.get(fog_id), endpoints[ep]]
    end if response && response.code == 200
    servers.each do |key, val|
      server = val[1]
      rebar_id = val[0]
      ep = val[2]
      kp_name = "id-fogwrap-#{rebar_id}"
      kp_loc = File.expand_path("~/.ssh/#{kp_name}")
      if ! File.exists?(kp_loc)
        ssh_key=Diplomat::Kv.get("fogwrap/keys/#{rebar_id}")
        File.open(kp_loc,"w") do |f|
          f.write(ssh_key)
          f.flush
          f.chmod(0600)
        end
      end
      log "Testing server #{server.id}"
      unless server.ready?
        log "Server #{server.id} not ready, skipping"
        next
      end
      server.private_key_path = kp_loc
      unless %w(rebar ec2-user ubuntu centos root).find do |user|
               server.username = user
               server.sshable? rescue false
             end
        log "Server #{server.id} not sshable, skipping"
        next
      end
      log "Adding rebar keys and enabling SSH in as root"
      server.ssh("sudo -- mkdir -p /root/.ssh")
      server.ssh("sudo -- sed -i -r '/(PasswordAuthentication|PermitRootLogin)/d' /etc/ssh/sshd_config")
      server.ssh("sudo -- printf '\nPasswordAuthentication %s\nPermitRootLogin %s\n' no yes >> /etc/ssh/sshd_config")
      server.ssh("sudo -- service ssh restart")
      server.ssh("sudo -- service sshd restart")
      Tempfile.open("fogwrap-keys") do |f|
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
      log("Adding node control address #{server.public_ip_address} to node #{rebar_id}")
      system("rebar nodes set #{rebar_id} attrib node-control-address to '{\"value\": \"#{server.public_ip_address}\"}'")
      log "Marking server #{server.id} alive"
      Diplomat::Kv.delete(key)
      Diplomat::Kv.delete("fogwrap/keys/#{rebar_id}")
      system("rebar nodes update #{rebar_id} '{\"alive\": true, \"available\": true}'")
      if ep.respond_to? :key_pairs
        old_kp = ep.key_pairs.get(kp_name)
        old_kp.destroy if old_kp
        File::delete(kp_loc, "#{kp_loc}.pub")
      end
      File::delete(kp_loc, "#{kp_loc}.pub")
    end
  rescue Exception => e
    log "Caught error, looping"
    log "Exception: #{e.message}"
    log e.backtrace
  end
  sleep 10
end
