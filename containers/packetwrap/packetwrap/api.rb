#!/usr/bin/env ruby

require 'json'
require 'jimson' # This is a JSONRPC 2.0 service
require 'rest-client'
require 'puma'
require 'diplomat'

class Servers
  extend Jimson::Handler

  def create(endpoint, node_id, args)
    log("Creating node #{node_id}")
    fixed_args = fix_hash(args)

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
      return e.inspect
    end
    if response.code != 201
      raise "Failed to create node for #{endpoint} #{fixed_args}"
    end

    device_data = JSON::parse(response.body)

    log("Created server #{device_data.to_json}")
    begin
      Diplomat::Kv.put("packetwrap/create/#{node_id}/#{device_data['id']}",endpoint.to_json)
    rescue Exception => e
      return "consul update failed: #{e.inspect}"
    end
    device_data
  end

  def list(endpoint)
    log("Get all servers")
    # Packet endpoint has the account key
    packet_project_token = endpoint['project_token']
    packet_project_id = endpoint['project_id']

    response = RestClient.get "https://api.packet.net/projects/#{packet_project_id}/devices",
                               content_type: :json, accept: :json, 
                               'X-Auth-Token' => packet_project_token

    if response.code != 200
      return "Failed to list devices for #{endpoint}"
    end

    data = JSON.parse(response.body)
    data['devices']
  end

  def get(endpoint, id)
    log("Get server #{id}")
    # Packet endpoint has the account key
    packet_project_token = endpoint['project_token']
    packet_project_id = endpoint['project_id']

    response = nil
    begin
      response = RestClient.get "https://api.packet.net/projects/#{packet_project_id}/devices/#{id}",
                                 content_type: :json, accept: :json, 
                                 'X-Auth-Token' => packet_project_token
    rescue Exception => e
      return e.inspect
    end

    if response.code != 200
      return "Failed to get device for #{endpoint} #{id}"
    end

    JSON::parse(response.body)
  end

  def reboot(endpoint, id)
    log("Rebooting server #{id}")
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
      return e.inspect
    end
    if response.code != 202
      raise "Failed to reboot node for #{endpoint} #{fixed_args}"
    end

    true
  end

  def delete(endpoint,id)
    log("Deleting server #{id}")

    # Packet endpoint has the account key
    packet_project_token = endpoint['project_token']
    packet_project_id = endpoint['project_id']

    response = nil
    begin
      response = RestClient.delete "https://api.packet.net/projects/#{packet_project_id}/devices/#{id}",
                                 content_type: :json, accept: :json, 
                                 'X-Auth-Token' => packet_project_token
    rescue Exception => e
      return e.inspect
    end

    if response.code != 204
      raise "Failed to delete device for #{endpoint} #{id}"
    end

    true
  end

  def register(endpoint, user, keys)
    log("Registering endpoint")

    # Packet endpoint has the account key
    packet_project_token = endpoint['project_token']
    packet_project_id = endpoint['project_id']

    response = nil
    begin
      response = RestClient.get "https://api.packet.net/ssh-keys",
                                 content_type: :json, accept: :json, 
                                 'X-Auth-Token' => packet_project_token
    rescue Exception => e
      return "Failed: error = #{e}"
    end

    if response.code != 200
      return "Failed to get ssh-keys for #{endpoint}: #{response.code}"
    end

    # Is our key in it?
    s = File.open('/root/.ssh/id_rsa.pub', 'rb') { |f| f.read }
    s = s.strip

    found = false
    wrong = false
    key_id = nil
    keys = JSON.parse(response.body)['ssh_keys']
    keys.each do |key|
      next unless key['label'] == 'Packetwrap Key'
      found = true
      wrong = true if s != key['key']
      key_id = key['id'] if wrong
      break
    end

    data = {
      'label' => 'Packetwrap Key',
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
        return e.inspect
      end
      if response.code != 200
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
        return e.inspect
      end
      if response.code != 201
        raise "Failed to add key to list #{endpoint}"
      end
    end
    
    true
  end

  private

  def log(line)
    STDOUT.puts(line)
    STDOUT.flush
  end

  def fix_hash(h)
    res = {}
    h.each_key do |k|
      res[k.to_sym] = h[k]
    end
    res
  end

end

# Fire it up, boys
router = Jimson::Router.new
router.namespace('servers',Servers.new)
server = Jimson::Server.new(router, port: 3031, server: 'puma')
server.start
