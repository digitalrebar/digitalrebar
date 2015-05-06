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
class BarclampDns::MgmtServer < Role

  def on_node_bind(nr)
    # if not set, set the name to the deployment name.
    NodeRole.transaction do
      name = Attrib.get("dns-management-name",nr)
      unless name
        Attrib.set("dns-management-name",nr,nr.node.deployment.name)
      end
    end
  end

  def sysdata(nr)
    my_addr = nr.node.addresses(:v4_only).first
    raise "No address for the DNS Management Server" unless my_addr
    { 'dns-mgmt' => { 'server_address' => my_addr.to_s } }
  end

end
