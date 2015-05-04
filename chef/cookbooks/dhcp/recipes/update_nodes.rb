# Copyright 2011, Dell
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

provisioner_addr = node['crowbar']['provisioner']['server']['webservers'].first["address"]
node.normal['crowbar_wall'] ||= Mash.new
node.normal['crowbar_wall']['dhcp'] ||= Mash.new
node.normal['crowbar_wall']['dhcp']['clients'] ||= Mash.new
new_clients = {}

(node['crowbar']['dhcp']['clients'] || {} rescue {}).each do |mnode_name,dhcp_info|
  # Build DHCP, PXE, and ELILO config files for each system
  v4addr = IP.coerce(dhcp_info['v4addr'])
  nodeaddr = sprintf('%X',v4addr.address)
  bootenv = dhcp_info['bootenv']
  mac_list = dhcp_info['mac_addresses']
  new_clients[mnode_name] = {
    'v4addr' => dhcp_info['v4addr'],
    'nodeaddr' => nodeaddr,
    'mac_addresses' => mac_list,
  }

  Chef::Log.info("DHCP: #{mnode_name} Updating DHCP Entry")
  mac_list.each_index do |idx|
    if bootenv == 'local'
      dhcp_opts = []
    else
      dhcp_opts = [
'  if option arch = 00:06 {
      filename = "discovery/bootia32.efi";
   } else if option arch = 00:07 {
      filename = "discovery/bootx64.efi";
   } else {
      filename = "discovery/pxelinux.0";
   }',
       "next-server #{provisioner_addr}"]
    end

    dhcp_host "#{mnode_name}-#{idx}" do
      hostname mnode_name
      ipaddress v4addr.addr
      macaddress mac_list[idx]
      options dhcp_opts
      action :add
    end
  end
end

# Now that we have handled any updates we care about, delete any info about nodes we have deleted.
(node['crowbar_wall']['dhcp']['clients'].keys - new_clients.keys).each do |old_node_name|
  old_node = node['crowbar_wall']['dhcp']['clients'][old_node_name]
  mac_list = old_node['mac_addresses']
  mac_list.each_index do |idx|
    a = dhcp_host "#{old_node_name}-#{idx}" do
      hostname old_node_name
      ipaddress '0.0.0.0'
      macaddress mac_list[idx]
      action :nothing
    end
    a.run_action(:remove)
  end
end

node.normal['crowbar_wall']['dhcp']['clients']=new_clients
