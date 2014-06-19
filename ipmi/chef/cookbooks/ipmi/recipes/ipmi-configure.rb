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

include_recipe "utils"

unless node[:platform] == "windows" or ::File.exists?("/usr/sbin/ipmitool") or ::File.exists?("/usr/bin/ipmitool")
  package "ipmitool" do
    case node[:platform]
    when "ubuntu","debian"
      package_name "ipmitool"
    when "redhat","centos"
      package_name "OpenIPMI-tools"
    end
    action :install
  end
end

provisioner_server = (node[:crowbar][:provisioner][:server][:webserver] rescue nil)
Chef::Log.info "Using #{provisioner_server} provisioner server"
return unless provisioner_server
bmc="bmc-2013-10-22.tgz"

[bmc].each do |f|
  a = remote_file "/tmp/#{f}" do
    source "#{provisioner_server}/files/#{f}"
    action :nothing
  end
  a.run_action(:create)
end

a = bash "Extract #{bmc}" do
  code <<EOC
if [[ ! -x /usr/bin/bmc ]]; then
    cd /usr/bin; tar xzf /tmp/#{bmc} bmc
fi
EOC
  action :nothing
end
a.run_action(:run)

bmc_user     = node[:ipmi][:bmc_user]
bmc_password = node[:ipmi][:bmc_password]
use_dhcp     = node[:ipmi][:use_dhcp]

bmc_addresses  = node["crowbar"]["network"]["bmc"]["addresses"] rescue ["0.0.0.0/24"]
address = IP.coerce(bmc_addresses[0]) rescue IP.coerce("0.0.0.0/24")
bmc_address  = address.addr
bmc_netmask  = address.netmask

bmc_router   = node["crowbar"]["network"]["bmc"]["router"] rescue "0.0.0.0"
bmc_use_vlan = node["crowbar"]["network"]["bmc"]["use_vlan"] rescue false
bmc_vlan     = if bmc_use_vlan
                 node["crowbar"]["network"]["bmc"]["vlan"].to_s
               else
                 "off"
               end

node.set["crowbar_wall"] = {} if node["crowbar_wall"].nil?
node.set["crowbar_wall"]["status"] = {} if node["crowbar_wall"]["status"].nil?
if node["crowbar_wall"]["status"]["ipmi"].nil?
  node.set["crowbar_wall"]["status"]["ipmi"] = {}
  node.set["crowbar_wall"]["status"]["ipmi"]["user_set"] = false
  node.set["crowbar_wall"]["status"]["ipmi"]["address_set"] = false
end

# save  input attributes for posterity
node.set["crowbar_wall"]["status"]["ipmi"]["params"] = {}
node.set["crowbar_wall"]["status"]["ipmi"]["params"]["ipmi"] = node["ipmi"]
node.set["crowbar_wall"]["status"]["ipmi"]["params"]["bmc"]  = node["crowbar"]["network"]["bmc"]
node.set["crowbar_wall"]["status"]["ipmi"]["params"]["bmc"]["address"] = bmc_address
node.set["crowbar_wall"]["status"]["ipmi"]["params"]["bmc"]["netmask"] = bmc_netmask


unsupported = [ "KVM", "Bochs", "VMWare Virtual Platform", "VMware Virtual Platform", "VirtualBox" ]

if node[:ipmi][:bmc_enable]
  if bmc_address == "0.0.0.0" || bmc_router == "0.0.0.0"
    node.set["crowbar_wall"]["status"]["ipmi"]["messages"] = [ "Bad IP address specifications"]
    node.set[:ipmi][:bmc_enable] = false
    return
  end

  if unsupported.member?(node[:dmi][:system][:product_name])
    node.set["crowbar_wall"]["status"]["ipmi"]["messages"] = [ "Unsupported platform: #{node[:dmi][:system][:product_name]} - turning off ipmi for this node" ]
    node.set[:ipmi][:bmc_enable] = false
    return
  end

  unless (node["crowbar_wall"]["status"]["ipmi"]["address_set"] and node["crowbar_wall"]["status"]["ipmi"]["user_set"])
    node.set["crowbar_wall"]["status"]["ipmi"]["messages"] = []
    Chef::Log.info("loading ipmi modules")
    unless node[:platform] == "windows"
      ipmi_load "ipmi_load" do
        settle_time 30
        action :run
      end
    end
  end
  
  unless node[:platform] == "windows" or node["crowbar_wall"]["status"]["ipmi"]["address_set"]
    if use_dhcp
      ### lan parameters to check and set. The loop that follows iterates over this array.
      # [0] = name in "print" output, [1] command to issue, [2] desired value.
      lan_params = [
          [ "IP Address Source" ,"ipmitool lan set 1 ipsrc dhcp", "DHCP Address", 60 ]
      ]

      lan_params.each do |param|
        ipmi_lan_set "#{param[0]}" do
          command param[1]
          value param[2]
          settle_time param[3]
          action :run
        end
      end

      node.set["crowbar_wall"]["status"]["ipmi"]["address_set"] = true
    else
      ### lan parameters to check and set. The loop that follows iterates over this array.
      # [0] = name in "print" output, [1] command to issue, [2] desired value.
      lan_params = [
        [ "IP Address Source" ,"ipmitool lan set 1 ipsrc static", "Static Address", 10 ] ,
        [ "IP Address" ,"ipmitool lan set 1 ipaddr #{bmc_address}", bmc_address, 1 ] ,
        [ "Subnet Mask" , "ipmitool lan set 1 netmask #{bmc_netmask}", bmc_netmask, 1 ] ,
        [ "Default VLAN", "ipmitool lan set 1 vlan id #{bmc_vlan}", bmc_vlan, 10 ]
      ]

      lan_params << [ "Default Gateway IP", "ipmitool lan set 1 defgw ipaddr #{bmc_router}", bmc_router, 1 ] unless bmc_router.nil? || bmc_router.empty?

      lan_params.each do |param|
        ipmi_lan_set "#{param[0]}" do
          command param[1]
          value param[2]
          settle_time param[3]
          action :run
        end
      end
    end

    bmc_commands = [
      [ "BMC nic_mode", "/usr/bin/bmc nic_mode set dedicated", "/usr/bin/bmc nic_mode get", "dedicated", 10 ],
      [ "Dell BMC nic_mode", "ipmitool delloem lan set dedicated", "ipmitool delloem lan get", "dedicated", 10 ]
    ]

    if !@@is_admin
      bmc_commands.each do |param| 
        ipmi_bmc_command "bmc #{param[0]}" do
          command param[1]
          test param[2]
          value param[3]
          settle_time param[4]
          action :run
        end
      end
    end
  end

  unless node[:platform] == "windows" or node["crowbar_wall"]["status"]["ipmi"]["user_set"]
    ipmi_user_set "#{bmc_user}" do
      password bmc_password
      action :run
    end
  end

  unless node[:platform] == "windows"
    Chef::Log.info("unloading ipmi modules")
    ipmi_unload "ipmi_unload" do
      action :run
    end
  end

end

