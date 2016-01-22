#!/usr/bin/env ruby

require 'net/ssh'
require 'json'
require 'jimson' # This is a JSONRPC 2.0 service
require 'puma'
# We wrap the relevant bits of Fog to get our work done.
require 'fog'
require 'diplomat'
require './common'

class Servers
  extend Jimson::Handler

  def create(endpoint, node_id, args)
    log("Creating node #{node_id}")
    begin
      ep = get_endpoint(endpoint)
      fixed_args = fix_hash(args)
      kp_name = "id-fogwrap-#{node_id}"
      kp_loc = File.expand_path("~/.ssh/#{kp_name}")
      if ! system("ssh-keygen -t rsa -b 1024 -N '' -f '#{kp_loc}'")
        log("Failed to generate new key #{kp_loc}")
        raise "Failed to generate key"
      end
      kp = Net::SSH::KeyFactory.load_private_key(kp_loc)
      case endpoint["provider"]
      when 'AWS'
        old_kp = ep.key_pairs.get(kp_name)
        old_kp.destroy if old_kp
        res = ep.import_key_pair(kp_name,"#{kp.ssh_type} #{[kp.public_key.to_blob].pack('m0')}")
        if res.status != 200
          log("Failed to import #{kp_name} to Amazon")
          raise "Failed to import key"
        end
        fixed_args[:key_name]=kp_name
        my_name = (fixed_args[:hostname] ? fixed_args[:hostname] : "dr-#{Time.now.to_i}").split('.')[0]
        fixed_args[:tags] = {"rebar:node-id" => node_id.to_s,
                             "Name" => my_name }
        fixed_args[:flavor_id] ||= 't2.micro'
        unless fixed_args[:image_id]
          log("Setting default image to an Ubuntu 14.04 based image")
          # These are hvm:ebs images
          fixed_args[:image_id] = case ep.region
                                  when "us-west-1" then "ami-a88de2c8"
                                  when "us-west-2" then "ami-b4a2b5d5"
                                  when "us-east-1" then "ami-bb156ad1"
                                  when "us-gov-west-1" then "ami-d6bbd9f5"
                                  when "eu-west-1" then "ami-cd0fd6be"
                                  when "eu-central-1" then "ami-bdc9dad1"
                                  when "ap-southeast-1" then "ami-9e7dbafd"
                                  when "ap-southeast-2" then "ami-187a247b"
                                  when "ap-northeast-1" then "ami-7386a11d"
                                  when "sa-east-1" then "ami-5040fb3c"
                                  when "cn-north-1" then "ami-4264f87b"
                                  else
                                    raise "No idea what region #{ep.region} is"
                                  end
        end
        log("Region #{ep.region} -> image #{fixed_args[:image_id]}")
      when 'Google'
        name = (fixed_args[:hostname] ? fixed_args[:hostname] : "rebar-fogwrap-#{node_id}").split('.')[0]
        zone = fixed_args[:zone_name] || "us-central1-f"
        if fixed_args[:disks].nil? || fixed_args[:disks].empty?
          fixed_args[:disks] = [{'autoDelete' => 'true',
                                 'boot' => 'true',
                                 'type' => 'PERSISTENT',
                                 'initializeParams' =>  {
                                   'sourceImage' => 'projects/ubuntu-os-cloud/global/images/ubuntu-1504-vivid-v20151120'}}]
        end
        fixed_args[:username] = 'rebar'
        fixed_args[:public_key_path] = "#{kp_loc}.pub"
        fixed_args[:name] = name
        defaults = {machine_type: 'n1-standard-1',
                    zone_name: zone}
        fixed_args = defaults.merge(fixed_args)
      else
        log("No idea how to handle #{endpoint['provider']}")
        raise "No idea how to handle #{endpoint["provider"]}"
      end
      log("Will create new srver with #{fixed_args.inspect}")
      server = ep.servers.create(fixed_args)
      log("Created server #{server.to_json}")
      ret = nil
      case endpoint["provider"]
      when 'AWS'
        Diplomat::Kv.put("fogwrap/create/#{node_id}/#{server.id}",endpoint.to_json)
        ret = {id: server.id}
      when 'Google'
        Diplomat::Kv.put("fogwrap/create/#{node_id}/#{server.name}",endpoint.to_json)
        ret = {id: server.name}
      end
      Diplomat::Kv.put("fogwrap/keys/#{node_id}",kp.to_s)
      File::delete(kp_loc, "#{kp_loc}.pub")
      ret
    rescue Exception => e
      log("Exception Create: #{e.inspect}")
      raise e
    end
  end

  def list(endpoint)
    begin
      log("Listing: #{endpoint}")
      ep = get_endpoint(endpoint)
      return [] unless ep
      ep.servers if ep
    rescue Exception => e
      log("Error List: #{e.inspect}")
      raise e
    end
  end

  def get(endpoint, id)
    ep = get_endpoint(endpoint)
    ep.servers.get(id)
  end

  def reboot(endpoint, id)
    log("Rebooting server #{id}")
    get(endpoint,id).reboot
  end

  def delete(endpoint,id)
    begin
      log("Deleting server #{id}")
      ep = get_endpoint(endpoint)
      log("Could not find endpoint for: #{endpoint}") unless ep
      server = ep.servers.get(id) if ep
      log("Could not find server for: #{id}") unless server
      server.destroy if server
    rescue Exception => e
      log("Error Delete: #{e.inspect}")
      raise e
    end
  end

  def register(endpoint, user, keys)
    log("Registering endpoint #{endpoint} using #{keys}")
    ep = get_endpoint(endpoint)
    case endpoint["provider"]
    when "AWS"
      sg = ep.security_groups.get('default')
      # make sure port 22 is open in the first security group
      unless sg.ip_permissions.find do |ip_permission|
               ip_permission['ipRanges'].find { |ip_range| ip_range['cidrIp'] == '0.0.0.0/0' } &&
                 ip_permission['fromPort'] == 22 &&
                 ip_permission['ipProtocol'] == 'tcp' &&
                 ip_permission['toPort'] == 22
             end
        log "Allowing SSH access"
        sg.authorize_port_range(22..22)
      else
        log "SSH access already enabled"
      end
      unless sg.ip_permissions.find do |ip_permission|
               ip_permission['ipRanges'].find { |ip_range| ip_range['cidrIp'] == '0.0.0.0/0' } &&
                 ip_permission['fromPort'] == -1 &&
                 ip_permission['ipProtocol'] == 'icmp'
             end
        log "Allowing ICMP access"
        sg.authorize_port_range(-1..-1, ip_protocol: 'icmp')
      else
        log "ICMP access already enabled"
      end
    when "Google" then true
    else
      raise "No idea how to handle #{endpoint["provider"]}"
    end
  end

end

# Fire it up, boys
router = Jimson::Router.new
router.namespace('servers',Servers.new)
server = Jimson::Server.new(router, port: 3030, server: 'puma')
server.start
