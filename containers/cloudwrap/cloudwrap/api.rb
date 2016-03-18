#!/usr/bin/env ruby
# Copyright 2016, RackN Inc

require 'net/ssh'
require 'json'
require 'jimson' # This is a JSONRPC 2.0 service
require 'puma'
# We wrap the relevant bits of Fog to get our work done.
require 'fog'
require 'diplomat'
require './common'
# openstack uses the python API via CLI commands
require './openstack'  

class Servers
  extend Jimson::Handler

  def upload_key(endpoint, ep, node_id, fixed_args)
    kp_name = keyfile_name(node_id)
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
    when 'Google'
      fixed_args[:public_key_path] = "#{kp_loc}.pub"
    when 'OpenStack'
      openstack_addkey(endpoint, kp_name, "#{kp_loc}.pub")
    when 'Packet'
      # Packet endpoint has the account key
      packet_project_token = endpoint['project_token']
      packet_project_id = endpoint['project_id']

      # Is our key in it?
      s = File.open("#{kp_loc}.pub", 'rb') { |f| f.read }
      s = s.strip

      found, wrong, key_id = get_packet_key_info(packet_project_token, kp_name, s)

      data = {
        'label' => kp_name,
        'key' => s
      }

      if found && wrong
        log("Updating our key in the list: #{key_id}")
        response = nil
        begin
          response = RestClient.patch "https://api.packet.net/ssh-keys/#{key_id}",
                                       data.to_json,
                                       content_type: :json, accept: :json,
                                       'X-Auth-Token' => packet_project_token
        rescue Exception => e
          log("Failed to get keys: #{e.inspect}")
          return e.inspect
        end
        if response.code != 200
          log("Failed to update key to list #{endpoint}: #{response.code}")
          return "Failed to update key to list #{endpoint}: #{response.code}"
        end

      elsif !found
        log("Adding our key to the list")
        response = nil
        begin
          response = RestClient.post "https://api.packet.net/ssh-keys",
                                     data.to_json,
                                     content_type: :json, accept: :json,
                                     'X-Auth-Token' => packet_project_token
        rescue Exception => e
          log(e.inspect)
          return e.inspect
        end
        if response.code != 201
          log("Failed to add key to list #{endpoint}")
          raise "Failed to add key to list #{endpoint}"
        end
      end

      true
    end

    [ kp, kp_loc ]
  end

  def create(endpoint, node_id, args)
    log("Creating node #{node_id}")
    begin
      ep = get_endpoint(endpoint)
      fixed_args = fix_hash(args)

      kp, kp_loc = upload_key(endpoint, ep, node_id, fixed_args)

      case endpoint["provider"]
      when 'AWS'
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

        log("Will create new srver with #{fixed_args.inspect}")
        server = ep.servers.create(fixed_args)
        log("Created server #{server.to_json}")
      when 'Google'
        name = (fixed_args[:hostname] ? fixed_args[:hostname] : "rebar-cloudwrap-#{node_id}").split('.')[0]
        zone = fixed_args[:zone_name] || "us-central1-f"
        if fixed_args[:disks].nil? || fixed_args[:disks].empty?
          fixed_args[:disks] = [{'autoDelete' => 'true',
                                 'boot' => 'true',
                                 'type' => 'PERSISTENT',
                                 'initializeParams' =>  {
                                   'sourceImage' => 'projects/ubuntu-os-cloud/global/images/ubuntu-1504-vivid-v20151120'}}]
        end
        fixed_args[:username] = 'rebar'
        fixed_args[:name] = name
        defaults = {machine_type: 'n1-standard-1',
                    zone_name: zone}
        fixed_args = defaults.merge(fixed_args)

        log("Will create new srver with #{fixed_args.inspect}")
        server = ep.servers.create(fixed_args)
        log("Created server #{server.to_json}")
      when 'OpenStack'
        name = (fixed_args[:hostname] ? fixed_args[:hostname] : "rebar-cloudwrap-#{node_id}").split('.')[0]
        openstack_create(endpoint, name)
      when 'Packet'
        # Packet endpoint has the account key
        packet_project_token = endpoint['project_token']
        packet_project_id = endpoint['project_id']

        date = Time.now.strftime("%H%M%S")
        node = {
          "facility" => (fixed_args[:facility] ? fixed_args[:facility] : "ewr1"),
          "plan" => (fixed_args[:plan] ? fixed_args[:plan] : "baremetal_1"),
          "operating_system" => (fixed_args[:os] ? fixed_args[:os] : "ubuntu_14_04"),
          "hostname" => (fixed_args[:hostname] ? fixed_args[:hostname] : "pw-#{date}")
        }

        response = nil
        begin
          response = RestClient.post "https://api.packet.net/projects/#{packet_project_id}/devices",
                                     node.to_json,
                                     content_type: :json, accept: :json,
                                     'X-Auth-Token' => packet_project_token
        rescue Exception => e
          log("Failed to create packet: #{e.inspect}")
          return e.inspect
        end
        if response.code != 201
          log("Failed to create node for #{endpoint} #{fixed_args}")
          raise "Failed to create node for #{endpoint} #{fixed_args}"
        end

        server = JSON::parse(response.body)
        log("Created server #{server.to_json}")
      else
        log("No idea how to handle #{endpoint['provider']}")
        raise "No idea how to handle #{endpoint["provider"]}"
      end

      ret = nil
      case endpoint["provider"]
      when 'AWS'
        Diplomat::Kv.put("cloudwrap/create/#{node_id}/#{server.id}",endpoint.to_json)
        ret = {id: server.id}
      when 'Google'
        Diplomat::Kv.put("cloudwrap/create/#{node_id}/#{server.name}",endpoint.to_json)
        ret = {id: server.name}
      when 'Packet'
        Diplomat::Kv.put("cloudwrap/create/#{node_id}/#{server['id']}",endpoint.to_json)
        ret = server
      end
      Diplomat::Kv.put("cloudwrap/keys/#{node_id}",kp.to_s)
      File::delete(kp_loc, "#{kp_loc}.pub")
      ret
    rescue Exception => e
      log("Exception Create: #{e.inspect}")
      raise e
    end
  end

  def list(endpoint)
    begin
      case endpoint["provider"]
      when 'AWS', 'Google'
        log("Listing: #{endpoint}")
        ep = get_endpoint(endpoint)
        return [] unless ep
        ep.servers if ep
      when 'OpenStack'
        openstack_list(endpoint)
      when 'Packet'
        # Packet endpoint has the account key
        packet_project_token = endpoint['project_token']
        packet_project_id = endpoint['project_id']

        response = RestClient.get "https://api.packet.net/projects/#{packet_project_id}/devices",
                                   content_type: :json, accept: :json,
                                   'X-Auth-Token' => packet_project_token

        if response.code != 200
          log("Failed to list devices for #{endpoint}")
          return "Failed to list devices for #{endpoint}"
        end

        data = JSON.parse(response.body)
        data['devices']
      end
    rescue Exception => e
      log("Error List: #{e.inspect}")
      raise e
    end
  end

  def get(endpoint, id)
    case endpoint["provider"]
    when 'AWS', 'Google'
      ep = get_endpoint(endpoint)
      ep.servers.get(id)
    when 'OpenStack'
      openstack_get(endpoint, id)
    when 'Packet'
      # Packet endpoint has the account key
      packet_project_token = endpoint['project_token']
      packet_project_id = endpoint['project_id']

      response = nil
      begin
        response = RestClient.get "https://api.packet.net/projects/#{packet_project_id}/devices/#{id}",
                                   content_type: :json, accept: :json,
                                   'X-Auth-Token' => packet_project_token
      rescue Exception => e
        log(e.inspect)
        return e.inspect
      end

      if response.code != 200
        log("Failed to get device for #{endpoint} #{id}")
        return "Failed to get device for #{endpoint} #{id}"
      end

      JSON::parse(response.body)
    end
  end

  def reboot(endpoint, id)
    log("Rebooting server #{id}")
    case endpoint["provider"]
    when 'AWS', 'Google'
      get(endpoint,id).reboot
    when 'OpenStack'
      openstack_reboot(endpoint,id)
    when 'Packet'
      # Packet endpoint has the account key
      packet_project_token = endpoint['project_token']
      packet_project_id = endpoint['project_id']

      action = "{ \"type\": \"reboot\" }"

      response = nil
      begin
        response = RestClient.post "https://api.packet.net/projects/#{packet_project_id}/devices/#{id}/actions",
                                   action,
                                   content_type: :json, accept: :json,
                                   'X-Auth-Token' => packet_project_token
      rescue Exception => e
        log(e.inspect)
        return e.inspect
      end
      if response.code != 202
        log("Failed to reboot node for #{endpoint} #{fixed_args}")
        raise "Failed to reboot node for #{endpoint} #{fixed_args}"
      end
      true
    end
  end

  def delete(endpoint,id)
    log("Deleting server #{id}")
    begin
      case endpoint["provider"]
      when 'AWS', 'Google'
        ep = get_endpoint(endpoint)
        log("Could not find endpoint for: #{endpoint}") unless ep
        server = ep.servers.get(id) if ep
        log("Could not find server for: #{id}") unless server
        server.destroy if server
      when 'OpenStack'
        openstack_delete(endpoint, id)
      when 'Packet'
        # Packet endpoint has the account key
        packet_project_token = endpoint['project_token']
        packet_project_id = endpoint['project_id']

        response = nil
        begin
          response = RestClient.delete "https://api.packet.net/projects/#{packet_project_id}/devices/#{id}",
                                     content_type: :json, accept: :json,
                                     'X-Auth-Token' => packet_project_token
        rescue Exception => e
          log(e.inspect)
          return e.inspect
        end

        if response.code != 204
          log("Failed to delete device for #{endpoint} #{id}")
          raise "Failed to delete device for #{endpoint} #{id}"
        end
        true
      end
    rescue Exception => e
      log("Error Delete: #{e.inspect}")
      raise e
    end
  end

  def register(endpoint, user, keys)
    log("Registering endpoint #{endpoint} using #{keys}")
    ep = get_endpoint(endpoint)
    case endpoint["provider"]
    when 'AWS'
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
    when 'Google' then true
    when 'OpenStack' then true
    when 'Packet' then true
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
