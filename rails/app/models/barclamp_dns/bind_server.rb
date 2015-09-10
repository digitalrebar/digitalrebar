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
class BarclampDns::BindServer < Role

  def template
    # this is a workable solution for now, we use the admin node to determine domain (except when non-exists!)
    domain = Node.admin.first.name.split(".",2)[1] rescue I18n.t('not_set')
    {"rebar" => {     "dns" => {
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

  def on_node_bind(nr)
    # if not set, set the name to the deployment name.
    NodeRole.transaction do
      name = Attrib.get("dns-svc-name",nr)
      unless name
        Attrib.set("dns-svc-name",nr,nr.node.deployment.name)
      end
    end
  end

  def sysdata(nr)
    my_addr = nr.node.addresses(:v4_only).first
    raise "No address for the DNS Server" unless my_addr
    { "dns" => { "service_address" => my_addr.to_s },
      "rebar" => {
        "dns" => {
          "nameservers" => nr.node.addresses.flatten.sort.map{|a|a.addr}
        }
      }
    }
  end
end
