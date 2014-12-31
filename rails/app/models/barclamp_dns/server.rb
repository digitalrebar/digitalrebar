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

require 'json'
class BarclampDns::Server < Role

  def template
    # this is a workable solution for now, we use the admin node to determine domain (except when non-exists!)
    domain = Node.admin.first.name.split(".",2)[1] rescue I18n.t('not_set')
    {"crowbar" => {     "dns" => {
                        "domain" => domain,
                        "contact" => "support@localhost.localdomain",
                        "forwarders" =>  [],
                        "static" => {},
                        "ttl" => "1h",
                        "slave_refresh" => "1d",
                        "slave_retry" => "2h",
                        "slave_expire" => "4w",
                        "negative_cache" => 300}}}
  end

  def sysdata(nr)
    {"crowbar" => {
        "dns" => {
          "nameservers" => nr.node.addresses.flatten.sort.map{|a|a.addr}
        }
      }
    }
  end
end
