# Copyright 2013, Dell 
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

class BarclampChef::Server < Role

  def sysdata(nr)
    addr = nr.node.addresses.detect{|addr|addr.v4?}.addr
    port = Attrib.get("chef-server_port",nr.role)
    protocol = Attrib.get("chef-server_protocol",nr.role)
    { "chefjig" => {
        "server" => {
          "fqdn" => nr.node.name,
          "address" => addr,
          "url" => "#{protocol}://#{addr}:#{port}"
        }
      }
    }
  end

  def on_active(nr)
    j = BarclampChef::Jig.where(:name => "chef").first
    j.server = Attrib.get("chef-server_url",nr)
    j.client_name = Attrib.get("chef-server_admin_client_name",nr)
    j.active = (Rails.env.development? ? false : true )
    j.key = "/home/crowbar/.chef/#{j.client_name}.pem"
    j.save!
  end

end
