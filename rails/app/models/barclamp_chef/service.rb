# Copyright 2015, Greg Althaus
# 
# Licensed under the Apache License, Version 2.0 (the "License"); 
# you may not use this file except in compliance with the License. 
# You may obtain a copy of the License at 
# 
#  http://www.apache.org/licenses/LICENSE-2.0 
# 
# Unless required by applicable law or agreed to in writing, software 
# distributed under the License is distributed on an "AS IS" BASIS, 
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
# See the License for the specific language governing permissions and 
# limitations under the License. 
# 

class BarclampChef::Service < Service

  def do_transition(nr, data)
    internal_do_transition(nr, data, "chef-service", "chef-servers") do |s|
      str_addr = s.ServiceAddress
      str_addr = s.Address if str_addr.nil? or str_addr.empty?
      Rails.logger.debug("ChefServer: #{s.inspect} #{str_addr}")
      addr = IP.coerce(str_addr)
      Rails.logger.debug("ChefServer: #{addr.inspect}")

      # TODO: THIS NEEDS TO RUN ON ALL RUNNERS AND API SERVERS
      server_name = s.ServiceTags.first
      proto = ConsulAccess.getKey("digitalrebar/private/chef/#{server_name}/proto")
      key = ConsulAccess.getKey("digitalrebar/private/chef/#{server_name}/pem")
      account = ConsulAccess.getKey("digitalrebar/private/chef/#{server_name}/account")

      url = "#{proto}://"
      if addr.v6?
        url << "[#{addr.addr}]"
      else
        url << addr.addr
      end
      url << ":#{s.ServicePort}"

      # Update knife file and key file
      knife_contents = "
log_level                :info
log_location             STDOUT
node_name                '#{account}'
client_key               '/home/rebar/.chef/#{account}.pem'
chef_server_url          '#{url}'
syntax_check_cache_path  '/home/rebar/.chef/syntax_check_cache'
ssl_verify_mode          :verify_none
"
      system("mkdir -p /home/rebar/.chef")
      File.open("/home/rebar/.chef/knife.rb", 'w', 0600) {|f| f.write(knife_contents) }
      File.open("/home/rebar/.chef/#{account}.pem", 'w', 0600) {|f| f.write(key) }

      # Make sure chef-server has code
      system("/opt/digitalrebar/core/bin/chef-cookbook-upload >/tmp/chef-upload.out 2>&1")

      j = BarclampChef::Jig.where(:name => "chef").first
      j.server = url
      j.client_name = account
      j.active = (Rails.env.development? ? false : true )
      j.key = "/home/rebar/.chef/#{account}.pem"
      j.save!

      url
    end
  end

end
