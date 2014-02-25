#
# Copyright (c) 2011 Dell Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# Note : This script runs on both the admin and compute nodes.
# It intentionally ignores the bios->enable node data flag.

nets = node[:crowbar][:network] || return
nets[:bmc] && nets[:admin] || return

address = node.address("bmc",::IP::IP4)
bmc_address  = address.addr
bmc_subnet = address.network.addr
bmc_netmask  = address.netmask
nat_address   = node["crowbar"]["network"]["bmc"]["router"] rescue "0.0.0.0"
Chef::Log.info "BMC address: #{address.inspect}"

my_address = node.address("admin",::IP::IP4)
Chef::Log.info "Node address: #{my_address.inspect}"
admin_subnet = my_address.network
admin_netmask = my_address.netmask

# no natting needed if host and bmc addresses in the same subnet
return if admin_subnet == bmc_subnet && admin_netmask == bmc_netmask

bash "Add route to get to our BMC via nat" do
  code "ip route add #{bmc_subnet}/#{bmc_netmask} via #{nat_address}"
  not_if "ip route show via #{nat_address} |grep -q #{bmc_subnet}"
end
