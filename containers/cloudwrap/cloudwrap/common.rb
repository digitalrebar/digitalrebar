#!/usr/bin/env ruby
# Copyright 2016, RackN Inc


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

def keyfile_name(node_id)
  "id-cloudwrap-#{node_id}-#{ENV['DR_START_TIME']}"
end

def get_packet_key_info(packet_project_token, kp_name, s)
  response = nil
  begin
    response = RestClient.get "https://api.packet.net/ssh-keys",
                               content_type: :json, accept: :json,
                               'X-Auth-Token' => packet_project_token
  rescue Exception => e
    log("Failed to get key info: error = #{e.inspect}")
    return [ false, true, nil ]
  end

  if response.code != 200
    log("Failed to get key info: resp = #{response.inspect}")
    return [ false, true, nil ]
  end

  found = false
  wrong = false
  key_id = nil
  keys = JSON.parse(response.body)['ssh_keys']
  keys.each do |key|
    next unless key['label'] == kp_name
    found = true
    wrong = true if s != key['key']
    key_id = key['id'] if wrong
    break
  end

  [ found, wrong, key_id ]
end

#
# Debug / Mock to test scale
#
# ep['debug'] = {
#   'host_ip'    => 'ip of fake host - should be sshable with rebar key - use docker node',
#   'boot_delay_time' => 300,
#   'ssh_delay_time' => 300,
# }
#

class FakeDriver
   @@servers = nil
   @@keypairs = nil

   def initialize(ep)
     @end_point = ep
     unless @@servers
       @@servers = Servers.new(self)
     end
     unless @@keypairs
       @@keypairs = KeyPairs.new
     end
     @ss = @@servers
     @kp = @@keypairs
   end

   class KeyPairs < Hash
     def get(id)
       self[id]
     end
   end

   class Servers < Hash
     def initialize(pp)
       @parent = pp
     end
     def get(id)
       s = self[id]
       unless s
         arr = id.split('-')
         node_id = arr[0]
         time = arr[1]
         s = Server.new(self, { :tags => { "rebar:node-id" => node_id } }, time)
         self[id] = s
       end
       s
     end

     def create(opts)
       s = Server.new(self, opts)
       self[s.name] = s
       s
     end

     def get_endpoint_address
       @parent.endpoint_data['debug']['host_ip']
     end

     def get_endpoint_boot_delay
       @parent.endpoint_data['debug']['boot_delay_time']
     end

     def get_endpoint_ssh_delay
       @parent.endpoint_data['debug']['ssh_delay_time']
     end
   end

   class Server
     def initialize(parent, opts, time = Time.now.to_i.to_s)
       @parent = parent
       @hostname = opts[:hostname]
       @nameid = "#{opts[:tags]["rebar:node-id"]}-#{time}"
       @create_time = time.to_i
     end

     def reboot
       true
     end

     def id
       @nameid
     end

     def name
       @nameid
     end

     def destroy
       @parent.delete(name)
       true
     end

     def ready?
       (@create_time + @parent.get_endpoint_boot_delay) < Time.now.to_i
     end

     def private_key_path=(s)
     end

     def username=(s)
     end

     def sshable?
       (@create_time + @parent.get_endpoint_ssh_delay) < Time.now.to_i
     end

     def ssh(command)
       true
     end

     def scp(path, command)
       true
     end

     def public_ip_address
       @parent.get_endpoint_address
     end

     def private_ip_address
       @parent.get_endpoint_address
     end
   end

   class Status
     def initialize(res)
       @result = res
     end

     def status
       @result
     end
   end

   def servers
     @ss
   end

   def endpoint_data
     @end_point
   end

   def key_pairs
     @kp
   end

   def import_key_pair(name, key)
     @kp[name] = key
     Status.new(200)
   end

   def region
     'us-west-1'
   end

end


def get_endpoint(ep)
  return FakeDriver.new(ep) if ep['debug']

  case ep['provider']
  when 'AWS' then
    eph = fix_hash(ep)
    eph.delete(:subnet_id) if eph[:subnet_id]
    Fog::Compute.new(eph)
  when 'Google'
    unless ep['google_json_key']
      log("Google requires the JSON authentication token at google_json_key")
      raise "Cannot authenticate Google endpoint"
    end
    res = fix_hash(ep)
    res[:google_json_key_string] = JSON.generate(res.delete(:google_json_key))
    Fog::Compute.new(res)
  when 'OpenStack' then nil
  when 'Packet' then nil
  else
    log("Cannot get endpoint for #{ep['provider']}")
  end
end
