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

unless node[:ipmi][:bmc_enable]
  Chef::Log.info("IPMI not enabled by ipmi-discover, refusing to do anything.")
  return
end

node.set["crowbar_wall"] ||= Mash.new
node.set["crowbar_wall"]["status"] ||= Mash.new
node.set["crowbar_wall"]["status"]["ipmi"] ||= Mash.new
node.set["crowbar_wall"]["status"]["ipmi"]["messages"] ||= []

ipmiinfo = IPMI.mc_info
lan_current_cfg = IPMI.laninfo
bmc_user     = node[:ipmi][:bmc_user]
bmc_password = node[:ipmi][:bmc_password]
chan         = lan_current_cfg["lan_channel"]
bmc_userid   = (bmc_user == "root") ? "2" : "3"

# This makes username/password setting half-idempotent.
# Full idempotency would detect when usernames/passwords
# changed on the BMC, and fixing them along with their rights.
# We don't do the second half for now.'
ruby_block "Signal success in setting user creds" do
  block do
    salt = rand(65536)
    hash = Digest::SHA1.new.base64digest("#{salt}:#{bmc_user}:#{bmc_password}")
    node.set["crowbar_wall"]["status"]["ipmi"]["user_salt"] = salt
    node.set["crowbar_wall"]["status"]["ipmi"]["user_hash"] = hash
  end
  action :nothing
end

bash "Set IPMI credentials and enable LAN channel access" do
  code <<EOC
set -e -x
ipmitool user set name #{bmc_userid} #{bmc_user}
ipmitool user set password #{bmc_userid} #{bmc_password}
ipmitool user priv #{bmc_userid} 4 #{chan}
ipmitool channel setaccess #{chan} #{bmc_userid} callin=on link=on ipmi=on privilege=4
ipmitool user enable #{bmc_userid}
EOC
  notifies :create, "ruby_block[Signal success in setting user creds]"
  not_if {
    salt = (node["crowbar_wall"]["status"]["ipmi"]["user_salt"] || 0 rescue 0)
    hash = Digest::SHA1.new.base64digest("#{salt}:#{bmc_user}:#{bmc_password}")
    hash == (node["crowbar_wall"]["status"]["ipmi"]["user_hash"] || "" rescue "")
  }
end

# If this is a Dell system, tell the BMC that we want to use the dedicated nic.
if node["dmi"]["system"]["manufacturer"] =~ /^Dell/
  bash "Set Dell BMC nic to dedicated mode" do
    code "ipmitool delloem lan set dedicated"
    not_if {%x{ipmitool delloem lan get}.strip == "dedicated" }
  end
end

if node[:ipmi][:use_dhcp]
  bash "Configure BMC to use DHCP" do
    code "ipmitool lan set #{chan} ipsrc dhcp"
    not_if { lan_current_cfg['ipsrc'] == "dhcp" }
  end
else
  lan_cfg = Mash.new
  lan_cfg['ipsrc'] = node[:ipmi][:use_dhcp] ? "dhcp" : "static"

  address = nil
  node[:crowbar][:network][:addresses].each do |addr,opts|
    next unless opts[:conduit] == "bmc"
    address = IP.coerce(addr)
    lan_cfg['vlan id'] = opts[:use_vlan] ? opts[:vlan].to_s : "off"
    lan_cfg['ipaddr'] = address.addr
    lan_cfg['netmask'] = address.netmask
    lan_cfg['defgw ipaddr'] = (node["crowbar"]["network"]["bmc"]["router"] || "0.0.0.0" rescue "0.0.0.0")
    break
  end
  unless address
    node.set["crowbar_wall"]["status"]["ipmi"]["messages"] <<= "Bad IP address specifications (#{address.inspect})"
    Chef::Log.error("Invalid IPv4 address #{address.inspect}")
    return
  end

  ['ipsrc','ipaddr','netmask','defgw ipaddr','vlan id'].each do |k|
    v = lan_cfg[k]
    cmd = "ipmitool lan set #{chan} #{k.to_s} #{v}"
    bash cmd do
      code cmd
      not_if {lan_current_cfg[k] == v}
    end
  end
end

ruby_block "Mark IPMI as configured" do
  block do
    node.set[:ipmi][:configured] = true
  end
end
