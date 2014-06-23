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

include_recipe "ipmi::ipmitool"

unsupported = [ "KVM", "Bochs", "VMWare Virtual Platform", "VMware Virtual Platform", "VirtualBox" ]
bmc_info_keys = {
  "Device ID" => "device_id",
  "Device Revision" => "device_rev",
  "Firmware Revision" => "firmware_rev",
  "IPMI Version" => "version",
  "Manufacturer ID" => "mfgr_id",
  "Manufacturer Name" => "mfgr_name",
  "Product ID" => "product_id",
  "Product Name" => "product_name",
  "Device Available" => "available",
  "Provides Device SDRs" => "provides_device_sdrs",
  "Additional Device Support" => "additional_devs" }

node.set[:ipmi][:bmc_enable] = false

return if (node[:platform] == "windows") || unsupported.member?(node[:dmi][:system][:product_name])

unless File.directory?("/sys/module/ipmi_devintf")
  %x{modprobe ipmi_si &>/dev/null}
  %x{modprobe ipmi_devintf &>/dev/null}
end
return unless File.directory?("/sys/module/ipmi_devintf")

ruby_block "discover ipmi settings" do
  block do
    ipmiinfo = Hash.new
    # Get some basic information about the IPMI comtroller.
    # Later on we will customize how we talk to the controller based
    # on the data we find here.
    # If we can't find an IPMI controller after 5 tries, give up.
    5.times do
      mcinfo = %x{ipmitool mc info}
      if $?.exitstatus == 0
        in_additional_devices = false
        mcinfo.lines.each do |line|
          k,v = line.split(':',2).map{|x|x.strip}
          case
          when line[0..3] == "    "
            ipmiinfo["additional_devs"] << line.strip if in_additional_devices
          when k == "Additional Device Support"
            in_additional_devices = true
            ipmiinfo[bmc_info_keys[k]]=[]
          when bmc_info_keys[k]
            in_additional_devices = false
            ipmiinfo[bmc_info_keys[k]] = v
          else
            in_additional_devices = false
          end
        end
        break
      else
        sleep 1
      end
    end
    unless ipmiinfo.empty?
      # Figure out what channel the LAN interface is on.
      # First result wins.
      laninfo = ''
      (1..11).each do |chan|
        tmp = %x{ipmitool lan print #{chan}}
        next unless $?.exitstatus == 0
        ipmiinfo['lan_channel'] = chan
        laninfo = tmp
        break
      end
      # If we never found a working LAN channel, we will not be able to use IPMI remotely.
      # Give up.
      if laninfo.empty? || laninfo =~ /is not a LAN channel/
        laninfo.lines.each do |line|
          k,v = line.split(':',2).map{|x|x.strip}
          case k
          when "IP Address" then ipmiinfo['address']=v
          when "Subnet Mask" then ipmiinfo['netmask']=v
          when "Default Gateway IP" then ipmiinfo['gateway']=v
          when "MAC Address" then ipmiinfo['macaddr']=v
          when "IP Address Source" then ipmiinfo['address_source']=v
          when "802.1q VLAN ID" then ipmiinfo['vlan']=v
          end
        end
        if node["dmi"]["system"]["manufacturer"] =~ /^Dell/
          ipmiinfo["lan_interface"]= %x{ipmitool delloem lan get}.strip
        end
        node.set[:crowbar_wall] ||= Mash.new
        node.set[:crowbar_wall][:ipmi] = ipmiinfo
        node.set[:ipmi][:bmc_enable] = true
      end
    end
  end
end
