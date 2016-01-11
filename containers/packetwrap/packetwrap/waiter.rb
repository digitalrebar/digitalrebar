#!/usr/bin/env ruby

require 'net/ssh'
require 'net/scp'
require 'json'
require 'rest-client'
require 'diplomat'
require 'base64'
require 'tempfile'

def fix_hash(h)
  res = {}
  h.each_key do |k|
    res[k.to_sym] = h[k]
  end
  res
end

def log(line)
  STDOUT.puts(line)
  STDOUT.flush
end

loop do
  begin
    log "Finding endpoints of servers to check"
    servers = {}
    response = RestClient.get('http://localhost:8500/v1/kv/packetwrap/create', params: {recurse: true}) rescue nil
    JSON.parse(response.body).each do |k|
      ep = fix_hash(JSON.parse(Base64.decode64(k["Value"])))
      packet_device_id = k["Key"].split("/",4)[-1]
      rebar_id = k["Key"].split("/",4)[-2]
      servers[k["Key"]] = [rebar_id, packet_device_id, ep]
    end if response && response.code == 200
    servers.each do |key, val|
      rebar_id = val[0]
      packet_device_id = val[1]
      ep = val[2]

      log "Testing server #{rebar_id} #{packet_device_id} state"
      response = nil
      begin
        response = RestClient.get "https://api.packet.net/projects/#{ep[:project_id]}/devices/#{packet_device_id}", content_type: :json, accept: :json, 'X-Auth-Token' => ep[:project_token]
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

      cidr = device_data['ip_addresses'][0]['cidr']
      dev_ip = device_data['ip_addresses'][0]['address']
 
      log "Make sure node is ssh-able?"
      begin
        Net::SSH.start(dev_ip, 'root', { paranoid: false }) do |ssh|
          # capture all stderr and stdout output from a remote process
          ssh.exec!("date")
        end
      rescue Exception => e
        log "Server #{packet_device_id} not sshable, skipping: #{e}"
        next
      end

      log "Put keys to node"
      Tempfile.open("packetwrap-keys") do |f|
        pubkeys = JSON.parse(`rebar deployments get system attrib rebar-access_keys`)
        pubkeys['value'].each_value do |v|
          f.puts(v.strip)
        end
        f.flush
        f.fsync
        begin
          Net::SCP.upload!(dev_ip, 'root', f.path, "/tmp/rebar_keys", { paranoid: false })
        rescue Exception => e
          log "Server #{packet_device_id} failed to upload keys, skipping: #{e}"
          next
        end
      end

      log "Put keys in place on node"
      begin
        Net::SSH.start(dev_ip, 'root', { paranoid: false }) do |ssh|
          # capture all stderr and stdout output from a remote process
          ssh.exec!("cat /tmp/rebar_keys >> /root/.ssh/authorized_keys")
          ssh.exec!("rm -f /tmp/rebar_keys")
        end
      rescue Exception => e
        log "Server #{packet_device_id} not key updatable, skipping: #{e}"
        next
      end

      log("Adding node control address #{dev_ip} to node #{rebar_id}")
      system("rebar nodes set #{rebar_id} attrib node-control-address to '{\"value\": \"#{dev_ip}/#{cidr}\"}'")
      log "Marking server #{packet_device_id} alive"
      Diplomat::Kv.delete(key)
      system("rebar nodes update #{rebar_id} '{\"alive\": true, \"available\": true}'")
    end
  rescue Exception => e
    log "Caught error, looping"
    log "Exception: #{e.message}"
    log e.backtrace
  end
  sleep 10
end
