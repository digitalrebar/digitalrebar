#
# Copyright (c) 2014 Victor Lowther
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

module IPMI
  UNSUPPORTED = [ "KVM", "Bochs", "VMWare Virtual Platform", "VMware Virtual Platform", "VirtualBox" ]
  BMC_INFO_KEYS =  {
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
    "Additional Device Support" => "additional_devs"
  }

  def self.supported?(node)
    return false if node[:platform] == "windows"
    return false if node[:dmi] && node[:dmi].empty?
    !UNSUPPORTED.member?(node[:dmi][:system][:product_name])
  end

  def self.ensure_access(node)
    return false unless node[:os] == "linux"
    unless File.directory?("/sys/module/ipmi_devintf")
      %x{modprobe ipmi_si &>/dev/null}
      %x{modprobe ipmi_devintf &>/dev/null}
    end
    File.directory?("/sys/module/ipmi_devintf")
  end

  def self.tool(node,command)
    sleep(5) unless node["quirks"].member?("ipmi-nodelay")
    cmd = "ipmitool #{command}"
    %x{ #{cmd} 2>&1 }
  end

  # Get some basic information about the IPMI comtroller.
  # Later on we will customize how we talk to the controller based
  # on the data we find here.
  def self.mc_info(node)
    res = Hash.new
    # If we can't find an IPMI controller after 5 tries, give up.
    5.times do
      mcinfo = tool(node,"mc info")
      if $?.exitstatus == 0
        in_additional_devices = false
        mcinfo.lines.each do |line|
          k,v = line.split(':',2).map{|x|x.strip}
          case
          when line[0..3] == "    "
            res["additional_devs"] << line.strip if in_additional_devices
          when k == "Additional Device Support"
            in_additional_devices = true
            res[BMC_INFO_KEYS[k]]=[]
          when BMC_INFO_KEYS[k]
            in_additional_devices = false
            res[BMC_INFO_KEYS[k]] = v
          else
            in_additional_devices = false
          end
        end
        break
      else
        sleep 1
      end
    end
    res
  end

  def self.laninfo(node)
    res = Hash.new
    # Figure out what channel the LAN interface is on.
    # First result wins.
    laninfo = ''
    (1..11).each do |chan|
      tmp = tool(node,"lan print #{chan}")
      next unless $?.exitstatus == 0
      res['lan_channel'] = chan
      laninfo = tmp
      break
    end
    # If we never found a working LAN channel, we will not be able to use IPMI
    # remotely. Give up.
    return res if laninfo.empty? || laninfo =~ /is not a LAN channel/
    laninfo.lines.each do |line|
      k,v = line.split(':',2).map{|x|x.strip}
      case k
      when "IP Address" then         res['ipaddr']=v
      when "Subnet Mask" then        res['netmask']=v
      when "Default Gateway IP" then res['defgw ipaddr']=v
      when "MAC Address" then        res['macaddr']=v
      when "IP Address Source"
        res['ipsrc'] = case v
                                     when "Static Address" then "static"
                                     when "DHCP Address" then "dhcp"
                                     when "BIOS Address" then "bios"
                                     else
                                       "none"
                                     end
      when "802.1q VLAN ID"
        res['vlan id'] = case
                           when v =~ /^[0-9]+$/ then v
                           else "off"
                           end
      end
    end
    res
  end
end
