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

node.set["rebar_wall"] ||= Mash.new
node.set["rebar_wall"]["status"] ||= Mash.new
node.set["rebar_wall"]["status"]["ipmi"] ||= Mash.new
node.set["rebar_wall"]["status"]["ipmi"]["messages"] ||= []
node.set["rebar_wall"]["status"]["ipmi"]["configured"] = false

ipmiinfo = IPMI.mc_info(node)
lan_current_cfg = IPMI.laninfo(node)
bmc_user     = node[:ipmi][:bmc_user]
bmc_password = node[:ipmi][:bmc_password]
chan         = lan_current_cfg["lan_channel"]
bmc_userid   = (bmc_user == "root") ? "2" : "3"

if node["quirks"].member?("ipmi-immutable-root")
    bmc_userid = "3"
end

# This makes username/password setting half-idempotent.
# Full idempotency would detect when usernames/passwords
# changed on the BMC, and fixing them along with their rights.
# We don't do the second half for now.'
ruby_block "Signal success in setting user creds" do
  block do
    salt = rand(65536)
    hash = Digest::SHA1.new.base64digest("#{salt}:#{bmc_user}:#{bmc_password}")
    node.set["rebar_wall"]["status"]["ipmi"]["user_salt"] = salt
    node.set["rebar_wall"]["status"]["ipmi"]["user_hash"] = hash
  end
  action :nothing
end

cmd_list = []
cmd_list << "user set name #{bmc_userid} #{bmc_user}" unless node["quirks"].member?("ipmi-immutable-rootname")
cmd_list << "user set password #{bmc_userid} #{bmc_password}"
cmd_list << "user priv #{bmc_userid} 4 #{chan}"
cmd_list << "channel setaccess #{chan} #{bmc_userid} callin=on link=on ipmi=on privilege=4" unless node["quirks"].member?("ipmi-crossed-access-channels")
cmd_list << "user enable #{bmc_userid}"
cmd_list << "lan set #{chan} access on" unless node["quirks"].member?("ipmi-crossed-access-channels")

ruby_block "Set IPMI credentials and enable LAN channel access" do
  block do
    cmd_list.each do |cmd|
      IPMI.tool(node,cmd)
      raise "Failed to run #{cmd}" unless $?.exitstatus == 0
      node.set["rebar_wall"]["status"]["ipmi"]["configured"] = true
    end
  end
  notifies :create, "ruby_block[Signal success in setting user creds]"
  not_if {
    salt = (node["rebar_wall"]["status"]["ipmi"]["user_salt"] || 0 rescue 0)
    hash = Digest::SHA1.new.base64digest("#{salt}:#{bmc_user}:#{bmc_password}")
    hash == (node["rebar_wall"]["status"]["ipmi"]["user_hash"] || "" rescue "")
  }
end

# If this is a Dell system, tell the BMC that we want to use the dedicated nic.
if node["quirks"].member?("ipmi-dell-dedicated-nic")
  ruby_block "Set Dell BMC nic to dedicated mode" do
    block do
      IPMI.tool(node,"delloem lan set dedicated")
      raise "Unable to set IPMI to dedicated nic mode" unless $?.exitstatus == 0
      node.set["rebar_wall"]["status"]["ipmi"]["configured"] = true
    end
    not_if { IPMI.tool(node,"delloem lan get").strip == "dedicated" }
  end
end

if node[:ipmi][:use_dhcp]
  ruby_block "Configure BMC to use DHCP" do
    block do
      IPMI.tool(node,"lan set #{chan} ipsrc dhcp")
      raise "Could not set IPMI to use DHCP" unless $?.exitstatus == 0
      node.set["rebar_wall"]["status"]["ipmi"]["configured"] = true
    end
  end unless lan_current_cfg['ipsrc'] == "dhcp"
else
  lan_cfg = Mash.new
  lan_cfg['ipsrc'] = "static"

  address = nil
  node[:ipmi][:network].each do |addr,opts|
    address = IP.coerce(addr)
    lan_cfg['vlan id'] = opts[:use_vlan] ? opts[:vlan].to_s : "off"
    lan_cfg['ipaddr'] = address.addr
    lan_cfg['netmask'] = address.netmask
    lan_cfg['defgw ipaddr'] = (IP.coerce(opts["router"]["address"]).addr || "0.0.0.0" rescue "0.0.0.0")
    node.set["rebar_wall"]["status"]["ipmi"]["configured"] = true
    break
  end
  unless address
    node.set["rebar_wall"]["status"]["ipmi"]["messages"] <<= "Bad IP address specifications (#{address.inspect})"
    Chef::Log.error("Invalid IPv4 address #{address.inspect}")
    return
  end

  ['ipsrc','ipaddr','netmask','defgw ipaddr','vlan id'].each do |k|
    v = lan_cfg[k]
    next if lan_current_cfg[k] == v
    cmd = "lan set #{chan} #{k.to_s} #{v}"
    ruby_block cmd do
      block do
        IPMI.tool(node,cmd)
        raise "Could not #{cmd}" unless $?.exitstatus == 0
        node.set["rebar_wall"]["status"]["ipmi"]["configured"] = true
      end
    end
  end
end

if node["quirks"].member?("ipmi-hard-reset-after-config") &&
   node["rebar_wall"]["status"]["ipmi"]["configured"]
  ruby_block "Hard reset IPMI controller after config has finished" do
    sleep 30
    IPMI.tool(node,"mc reset cold")
    sleep 30
  end
end

ruby_block "Mark IPMI as configured" do
  block do
    node.set[:ipmi][:configured] = true
  end
end
