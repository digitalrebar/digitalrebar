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
nets[:bmc] && nets[:admin]  || return

bmc_addresses  = nets["bmc"]["addresses"] rescue ["0.0.0.0/24"]
address = IP.coerce(bmc_addresses[0]) rescue IP.coerce("0.0.0.0/24")
bmc_subnet = address.network.addr
bmc_netmask  = address.netmask

address = node.address("admin",::IP::IP4)
admin_subnet = address.network.addr
admin_netmask  = address.netmask

bash "Set up masquerading for the BMC network" do
  code <<EOC
iptables -t nat -F POSTROUTING
iptables -t nat -A POSTROUTING -s #{admin_subnet}/#{admin_netmask} -d #{bmc_subnet}/#{bmc_netmask} -j SNAT --to-source #{nets[:bmc][:router]}
iptables -P FORWARD DROP
iptables -F FORWARD
iptables -A FORWARD -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
iptables -A FORWARD -s #{admin_subnet}/#{admin_netmask} -d #{bmc_subnet}/#{bmc_netmask} -j ACCEPT
echo 1 >/proc/sys/net/ipv4/ip_forward
EOC
  not_if "iptables -t nat --list -n |grep #{nets[:bmc][:router]}"
end
