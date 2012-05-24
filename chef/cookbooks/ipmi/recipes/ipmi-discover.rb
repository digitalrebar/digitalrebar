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

unless ::File.exists?("/usr/sbin/ipmitool") or ::File.exists?("/usr/bin/ipmitool")
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

unsupported = [ "KVM", "Bochs", "VMWare Virtual Platform", "VMware Virtual Platform", "VirtualBox" ]

if node[:ipmi][:bmc_enable]
  if unsupported.member?(node[:dmi][:system][:product_name])
    node["crowbar_wall"] = {} unless node["crowbar_wall"]
    node["crowbar_wall"]["status"] = {} unless node["crowbar_wall"]["status"]
    node["crowbar_wall"]["status"]["ipmi"] = {} unless node["crowbar_wall"]["status"]["ipmi"]
    node["crowbar_wall"]["status"]["ipmi"]["messages"] = [ "Unsupported platform: #{node[:dmi][:system][:product_name]} - turning off ipmi for this node" ]
    node[:ipmi][:bmc_enable] = false
    node.save
    return
  end

  %x{modprobe ipmi_si ; modprobe ipmi_devintf ; sleep 15}
  %x{ipmitool lan print 1 > /tmp/lan.print}
  if $?.exitstatus == 0
    node["crowbar_wall"] = {} unless node["crowbar_wall"]
    node["crowbar_wall"]["ipmi"] = {} unless node["crowbar_wall"]["ipmi"]
    node["crowbar_wall"]["ipmi"]["address"] = %x{grep "IP Address   " /tmp/lan.print | awk -F" " '\{print $4\}'}.strip
    node["crowbar_wall"]["ipmi"]["gateway"] = %x{grep "Default Gateway IP " /tmp/lan.print | awk -F" " '\{ print $5 \}'}.strip
    node["crowbar_wall"]["ipmi"]["netmask"] = %x{grep "Subnet Mask" /tmp/lan.print | awk -F" " '\{ print $4 \}'}.strip
    node["crowbar_wall"]["ipmi"]["mode"] = %x{ipmitool delloem lan get}.strip
    node.save
  else
    node["crowbar_wall"] = {} unless node["crowbar_wall"]
    node["crowbar_wall"]["status"] = {} unless node["crowbar_wall"]["status"]
    node["crowbar_wall"]["status"]["ipmi"] = {} unless node["crowbar_wall"]["status"]["ipmi"]
    node["crowbar_wall"]["status"]["ipmi"]["messages"] = [ "Could not get IPMI lan info: #{node[:dmi][:system][:product_name]} - turning off ipmi for this node" ]
    node[:ipmi][:bmc_enable] = false
    node.save
    return
  end

  %x{rmmod ipmi_si ; rmmod ipmi_devintf ; rmmod ipmi_msghandler}
end

