# Copyright 2013, Dell
# Copyright 2012, SUSE Linux Products GmbH
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

# Calculate the "right" NIC ordering.
# First, find all the nics that register as PCI devices.

require 'digest/md5'

def split_pci(n)
  n.split(/[\.\/:]/).map{|i|i.to_i(16)}
end

net_re = /\/pci(.+)\/net\/(.+)$/

net_sysfs = "/sys/class/net"
raise "This recipe only works on Linux" unless File.directory?(net_sysfs)

# Sigh -- chef_gem behaviour is too flaky to rely on.
# Fake it if we are running the omnibus version of Chef.
if File.directory?("/opt/chef/embedded")
  if Dir.glob("/opt/chef/embedded/lib/ruby/gems/**/cstruct*/").empty?
    Chef::Log.info("Manually installing cstruct gem")
    unless system("/opt/chef/embedded/bin/gem install --no-ri --no-rdoc cstruct")
      raise "Could not install cstruct gem!"
    end
    Gem.clear_paths
  end
else
  chef_gem "cstruct"
end
require "socket"
require "cstruct"


# From: "/usr/include/linux/sockios.h"
SIOCETHTOOL = 0x8946

# From: "/usr/include/linux/ethtool.h"
ETHTOOL_GSET = 1
ETHTOOL_GLINK = 10

# From: "/usr/include/linux/ethtool.h"
class EthtoolCmd < CStruct
  uint32 :cmd
  uint32 :supported
  uint32 :advertising
  uint16 :speed
  uint8  :duplex
  uint8  :port
  uint8  :phy_address
  uint8  :transceiver
  uint8  :autoneg
  uint8  :mdio_support
  uint32 :maxtxpkt
  uint32 :maxrxpkt
  uint16 :speed_hi
  uint8  :eth_tp_mdix
  uint8  :reserved2
  uint32 :lp_advertising
  uint32 :reserved_a0
  uint32 :reserved_a1
end

SPEEDS = Array.new
# Bitfield for ETHTOOL_GSET results, by powers of 2 (just the speeds)
SPEEDS[0] = [10,   "10m"]   # 0:  10   baseT, half duplex
SPEEDS[1] = [10,   "10m"]   # 1:  10   baseT, full duplex
SPEEDS[2] = [100,  "100m"]  # 2:  100  baseT, half duplex
SPEEDS[3] = [100,  "100m"]  # 3:  100  baseT, full duplex
SPEEDS[4] = [1000, "1g"]    # 4:  1000 baseT, half duplex
SPEEDS[5] = [1000, "1g"]    # 5:  1000 baseT, full duplex
                            # 6:  Autonegotiate enabled
                            # 7:  TP connection
                            # 8:  AUI connection
                            # 9:  MII connection
                            # 10: Fibre connection
                            # 11: BNC connection
SPEEDS[12] = [10000,"10g"]  # 12: 10000 baseT, full duplex
                            # 13: Pause support
                            # 14: Asym Pause support
SPEEDS[15] = [2500,  "2g"]  # 15: 2500 baseX, full duplex
                            # 16: Backplane
SPEEDS[17] = [1000,  "1g"]  # 17: 1000 baseKX, full duplex
SPEEDS[18] = [10000,"10g"]  # 18: 10000 baseKX4, full duplex
SPEEDS[19] = [10000,"10g"]  # 19: 10000 baseKR, full duplex
SPEEDS[20] = [10000,"10g"]  # 20: 10000 baseR, FEC
SPEEDS[21] = [20000,"20g"]  # 21: 20000 baseMLD2, full duplex
SPEEDS[22] = [20000,"20g"]  # 22: 20000 baseKR2, full duplex
SPEEDS[23] = [40000,"40g"]  # 23: 40000 baseKR4, full duplex
SPEEDS[24] = [40000,"40g"]  # 24: 40000 baseCR4, full duplex
SPEEDS[25] = [40000,"40g"]  # 25: 40000 baseSR4, full duplex
SPEEDS[26] = [40000,"40g"]  # 26: 40000 baseLR4, full duplex

class EthtoolValue < CStruct
  uint32 :cmd
  uint32 :value
end

def nic_speeds(nic)
  begin
    ecmd = EthtoolCmd.new
    ecmd.cmd = ETHTOOL_GSET
    ifreq = [nic, ecmd.data].pack("a16p")
    sock = Socket.new(Socket::AF_INET, Socket::SOCK_DGRAM, 0)
    sock.ioctl(SIOCETHTOOL, ifreq)

    rv = ecmd.class.new
    rv.data = ifreq.unpack("a16p")[1]

    res = []
    SPEEDS.each_index do |i|
      next unless SPEEDS[i] && ((rv.supported & (1 << i)) != 0)
      res << SPEEDS[i]
    end
    res.sort.uniq.reverse.map{|i|i[1]}
  rescue Exception => e
    Chef::Log.error("Failed to get ioctl for speed: #{e.message}")
    [ "1g", "0g" ]
  end
end

# Get all of the network devices that are real physical devices.
#  We consider a real physical device to be anything that lives on a PCI bus.
nics = Hash[]
Dir.foreach(net_sysfs) do |ent|
  ent = File.join(net_sysfs,ent)
  next unless File.symlink?(ent)
  # We know this is a symlink to a real device.  Extract what we need.
  symlink = File.readlink(ent)
  matches = net_re.match(symlink)
  next unless matches && matches.length == 3
  Chef::Log.info("Found #{matches[1]} => #{matches[2]}")
  nics[split_pci(matches[1])] = matches[2]
end

# If we need to force ordering away from the way the PCI addresses would
# normally fall out on the system, here is where we do it.
forcing_ents = Array.new
node["rebar"]["interface_map"].each do |ent|
  next unless node[:dmi][:system][:product_name] =~ /#{ent["pattern"]}/
  Chef::Log.info("Using interface map override for #{ent["pattern"]}")
  ent["bus_order"].each do |i|
    forcing_ents << split_pci(i)
  end
  break
end if (node["rebar"]["interface_map"] rescue nil) && node[:dmi] && !node[:dmi].empty?

Chef::Log.info("Not using any forcing entries") if forcing_ents.empty?

bus_ents = nics.keys.sort
Chef::Log.info("Found nics: #{nics.inspect}")

# Bucketize the nics we found. This sorts the nics into at most
# forcing_ents.length + 1 buckets.
sorted_keys = Array.new()
nics.keys.each do |nic|
  found = false
  forcing_ents.each_index do |fi|
    # Check to see if the PCI address of this nic is one that
    # is covered by one of our interface maps.
    f = forcing_ents[fi]
    next unless nic[0,f.length] == f
    # It is.  Put it in the right bucket.
    sorted_keys[fi] ||= Array.new
    sorted_keys[fi] << nic
    found = true
    break
  end
  next if found
  # The PCI address of this nic was not matched by any interface maps.
  # Put it into the everything else bucket.
  sorted_keys[forcing_ents.length] ||= Array.new
  sorted_keys[forcing_ents.length] << nic
end

# Now, save our final sorted list by sorting each bucket by PCI address,
# then mapping the address back to a nic name, then flattening the whole list.
node.set["rebar"] ||= Mash.new
node.set["rebar"]["sorted_ifs"] = sorted_keys.compact.map{|e|
  e.sort.map{|e|
    nics[e]
  }
}.flatten

if ::File.exists?("/etc/init/network-interface.conf")
  # Make upstart stop trying to dynamically manage interfaces.
  ::File.unlink("/etc/init/network-interface.conf")
  ::Kernel.system("killall -HUP init")
end

# Stop udev from jacking up our vlans and bridges as we create them.
["40-bridge-network-interface.rules","40-vlan-network-interface.rules"].each do |rule|
  next if ::File.exists?("/etc/udev/rules.d/#{rule}")
  next unless ::File.exists?("/lib/udev/rules.d/#{rule}")
  ::Kernel.system("echo 'ACTION==\"add\", SUBSYSTEM==\"net\", RUN+=\"/bin/true\"' >/etc/udev/rules.d/#{rule}")
end

route_pref = 10000
ifs = Mash.new
old_ifs = node["rebar_wall"]["network"]["interfaces"] || Mash.new rescue Mash.new
if_mapping = Array.new

default_route = {}

# Silly little helper for sorting Rebar networks.
# Netowrks that use vlans and bridges will be handled later
def net_weight(net)
  res = 0
  if net["use_vlan"] then res += 1 end
  if net["use_bridge"] then res += 1 end
  res
end

# given a conduit definition, resolve it down to a set of physical interfaces.
# The supported reference format is <sign><speed><#> where
#  * sign is optional, and determines behavior if exact match is not found.
#    + allows speed upgrade,
#    - allows downgrade, and
#    ? allows either. If no sign is specified, an exact match must be found.
#  * speed designates the interface speed. 10m, 100m, 1g and 10g are supported
#  * The final number designates the zero-based offset into the set of physical
#    interfaces that have the requested speed we want.
def resolve_conduit(conduit)
  if node["rebar_ohai"]["in_docker"]
    return ["eth0"]
  end
  known_ifs = node["rebar"]["sorted_ifs"]
  speeds = %w{10m 100m 1g 10g}
  intf_re = /^([-+?]?)(\d{1,3}[mg])(\d+)$/
  finders = conduit.split(',').map{|f|f.strip}
  raise "#{conduit} does not want any interfaces!" if finders.nil? || finders.empty?
  finders = finders.map{|i|intf_re.match(i)}
  malformed = finders.find_all{|i|i.length != 4}
  raise "Malformed interface selectors: #{malformed}" unless malformed.empty?
  # At this point, the selectors are at least well-formed. Verify that they are sane.
  tmpl = finders[0]
  if ! finders.all? do |i|
      (i[1] == tmpl[1]) && (i[2] == tmpl[2])
    end
    raise "Interface selectors do not have the same speed and flags: #{conduit}"
  end
  # The conduit looks sane, but is it requesting a speed we know about?
  # Check and see.
  speed_idx = speeds.index(tmpl[2])
  raise "Unknown requested speed #{template[2]}" unless speed_idx
  # At this point, the conduit definition is sane.
  wanted_speeds = case tmpl[1]
                  when '+' then speeds[speed_idx..-1]
                  when "-" then speeds[0..speed_idx].reverse
                  when "?" then (speeds[speed_idx..-1] + (speeds[0..speed_idx].reverse)).uniq
                  else [tmpl[2]]
                  end
  # Now, loop over all the wanted speeds until we find the interfaces with the desired
  # offsets.
  wanted_speeds.each do |speed|
    candidates = known_ifs.select do |i|
      # Fastest speed is at the beginning, and that is all we care about comparing right now.
     nic_speeds(i).first == speed
    end
    res = finders.map{|f|candidates[f[3].to_i]}.compact
    if res.length == finders.length
      return res
    end
  end
  raise "Cannot resolve conduit #{conduit} with known interfaces #{known_ifs}"
end

# Dynamically create our new local interfaces.
node["rebar"]["network"]["addresses"].keys.sort{|a,b|
  net_weight(node["rebar"]["network"]["addresses"][a]) <=> net_weight(node["rebar"]["network"]["addresses"][b])
}.each do |addr|
  network = node["rebar"]["network"]["addresses"][addr]
  # Skip BMC conduits.
  next if network["conduit"] == "bmc"
  next if network["network"] =~ /^unmanaged-/
  # This will wind up being the interfaces that the address will be bound to.
  net_ifs = Array.new
  # This is the basic interfaces that the conduit definition implies we should use.
  base_ifs = resolve_conduit(network["conduit"]).map{|i| Nic.new(i)}.sort
  netname = "#{addr} in network range #{network['network']}:#{network['range']}"
  Chef::Log.info("Using base interfaces #{base_ifs.map{|i|i.name}.inspect} for #{netname}")
  base_ifs.each do |i|
    ifs[i.name] ||= Hash.new
    ifs[i.name]["addresses"] ||= Array.new
    ifs[i.name]["type"] = "physical"
    ifs[i.name]["speeds"] = nic_speeds(i.name)
  end
  case base_ifs.length
  when 1
    Chef::Log.info("Using interface #{base_ifs[0]} for #{netname}")
    our_iface = base_ifs[0]
  else
    # We want a bond.  Figure out what mode it should be.  Default to 5
    team_mode = network["team_mode"] || 5
    # See if a bond that matches our specifications has already been created,
    # or if there is an empty bond lying around.
    bond = Nic.nics.detect do|i|
      i.kind_of?(Nic::Bond) &&
        (i.slaves.empty? ||
         (i.slaves.sort == base_ifs))
    end
    if bond
      Chef::Log.info("Using bond #{bond.name} for #{netname}")
    else
      bond = Nic::Bond.create("bond#{Nic.nics.select{|i| Nic::bond?(i)}.length}",
                       team_mode)
      Chef::Log.info("Creating bond #{bond.name} for #{netname}")
    end
    ifs[bond.name] ||= Hash.new
    ifs[bond.name]["addresses"] ||= Array.new
    ifs[bond.name]["slaves"] = Array.new
    base_ifs.each do |i|
      bond.add_slave i
      ifs[bond.name]["slaves"] << i.name
      ifs[i.name]["slave"] = true
      ifs[i.name]["master"] = bond.name
    end
    ifs[bond.name]["mode"] = team_mode
    ifs[bond.name]["type"] = "bond"
    our_iface = bond
  end
  net_ifs << our_iface.name
  # If we want a vlan interface, create one on top of the base physical
  # interface and/or bond that we already have
  if network["use_vlan"]
    unless system("which vconfig")
      case node[:platform]
      when "ubuntu","debian","suse"
        p = package "vlan" do
          action :nothing
        end
      when "centos","redhat","fedora"
        p = package "vconfig" do
          action :nothing
        end
      end
      p.run_action :install
    end
    vlan = "#{our_iface.name}.#{network["vlan"]}"
    if Nic.exists?(vlan)
      Chef::Log.info("Using vlan #{vlan} for #{netname}")
      our_iface = Nic.new vlan
    else
      Chef::Log.info("Creating vlan #{vlan} for #{netname}")
      our_iface = Nic::Vlan.create(our_iface,network["vlan"])
    end
    # Destroy any vlan interfaces for this vlan that might
    # already exist
    Nic.nics.each do |n|
      next unless n.kind_of?(Nic::Vlan)
      next if n == our_iface
      next unless n.vlan == network["vlan"].to_i
      n.destroy
    end
    ifs[our_iface.name] ||= Hash.new
    ifs[our_iface.name]["addresses"] ||= Array.new
    ifs[our_iface.name]["type"] = "vlan"
    ifs[our_iface.name]["vlan"] = our_iface.vlan
    ifs[our_iface.name]["parent"] = our_iface.parents[0].name
    net_ifs << our_iface.name
  end
  # Ditto for a bridge.
  if network["use_bridge"]
    unless system("which brctl")
      p = package "bridge-utils" do
        action :nothing
      end
      p.run_action :install
    end

    bridge = if our_iface.kind_of?(Nic::Vlan)
               "br#{our_iface.vlan}"
             else
               "br-#{our_iface.name}"
             end
    br = if Nic.exists?(bridge)
           Chef::Log.info("Using bridge #{bridge} for #{netname}")
           Nic.new bridge
         else
           Chef::Log.info("Creating bridge #{bridge} for #{netname}")
           Nic::Bridge.create(bridge)
         end
    ifs[br.name] ||= Hash.new
    ifs[br.name]["addresses"] ||= Array.new
    ifs[our_iface.name]["slave"] = true
    ifs[our_iface.name]["master"] = br.name
    br.add_slave our_iface
    ifs[br.name]["slaves"] = [our_iface.name]
    ifs[br.name]["type"] = "bridge"
    our_iface = br
    net_ifs << our_iface.name
  end
  if_mapping << [network['network'],network['range'],addr,net_ifs.reverse,network['category']]
  ifs[our_iface.name]["addresses"] ||= Array.new
  ifs[our_iface.name]["addresses"] << IP.coerce(addr)
  ifs[our_iface.name]['pbr'] = network['pbr'] if network['pbr'] and network['pbr'] != ''
  ifs[our_iface.name]['router'] = IP.coerce(network["router"]["address"]).addr if network['router']

  # Ditto for our default route
  if network["router"] && network["router"]["pref"] && 
     (network["router"]["pref"].to_i < route_pref)
    Chef::Log.info("#{network["network"]}: Will use #{network["router"]} as our default route")
    route_pref = network["router"]["pref"].to_i
    default_route = {:nic => our_iface.name,
                     :gateway => IP.coerce(network["router"]["address"]).addr}
  end
end

# Kill any nics that we don't want hanging around anymore.
old_ifs.each do |name,params|
  next if ifs[name]
  Chef::Log.info("#{name} is no longer being used, deconfiguring it.")
  Nic.new(name).destroy if Nic.exists?(name)
  case node["platform"]
  when "centos","redhat","fedora"
    # Redhat and Centos have lots of small files definining interfaces.
    # Delete the ones we no longer care about here.
    if ::File.exists?("/etc/sysconfig/network-scripts/ifcfg-#{name}")
      ::File.delete("/etc/sysconfig/network-scripts/ifcfg-#{name}")
    end
    if ::File.exists?("/etc/sysconfig/network-scripts/route-#{name}")
      ::File.delete("/etc/sysconfig/network-scripts/route-#{name}")
    end
    if ::File.exists?("/etc/sysconfig/network-scripts/rule-#{name}")
      ::File.delete("/etc/sysconfig/network-scripts/rule-#{name}")
    end
  when "suse"
    # SuSE also has lots of small files, but in slightly different locations.
    if ::File.exists?("/etc/sysconfig/network/ifcfg-#{name}")
      ::File.delete("/etc/sysconfig/network/ifcfg-#{name}")
    end
    if ::File.exists?("/etc/sysconfig/network/ifroute-#{name}")
      ::File.delete("/etc/sysconfig/network/ifroute-#{name}")
    end
    ## TODO: PBR Work
  end
end

pbrs=[]
ifs.each do |n,i|
  pbrs << i['pbr'] if i['pbr']
end
pbrs = pbrs.sort
pbrs = pbrs.uniq

h={}
count=252
pbrs.each do |p|
  h[p] = count
  count = count - 1
end
pbrs = h

Nic.refresh_all

# At this point, any new interfaces we need have been configured, we know
# what IP addresses should be assigned to each interface, and we know what
# default route we should use. Make reality match our expectations.
Nic.nics.each do |nic|
  # If this nic is neither in our old config nor in our new config, skip
  next unless ifs[nic.name]
  iface = ifs[nic.name]
  old_iface = old_ifs[nic.name]
  # If we are a member of a bond or a bridge, then the bond or bridge
  # gets our config instead of us. The order in which Nic.nics returns
  # interfaces ensures that this will always function properly.
  if (master = nic.bond_master || nic.bridge_master)
    if iface["slave"]
      # We should continue to be a slave.
      Chef::Log.info("#{master.name}: usurping #{nic.name}")
      ifs[nic.name]["addresses"].each{|a|
        ifs[master.name]["addresses"] << a
      }
      ifs[nic.name]["addresses"] = []
      default_route[:nic] = master.name if default_route[:nic] == nic.name
      if_mapping.each { |a|
        v = a[3]
        v << master.name if v.last == nic.name
      }
    else
      # We no longer want to be a slave.
      Chef::Log.info("#{nic.name} no longer wants to be a slave of #{master.name}")
      master.remove_slave nic
    end
  end
  nic.up
  if nic.dhcp_pid
    Chef::Log.info("#{nic.name}: Taking over from dhcp")
    nic.flush
  end
    
  Chef::Log.info("#{nic.name}: current addresses: #{nic.addresses.map{|a|a.to_s}.sort.inspect}") unless nic.addresses.empty?
  Chef::Log.info("#{nic.name}: required addresses: #{iface["addresses"].map{|a|a.to_s}.sort.inspect}") unless iface["addresses"].empty?
  # Ditch old addresses, add new ones.
  old_iface["addresses"].reject{|i|iface["addresses"].member?(i)}.each do |addr|
    # Don't kill Docker's IP address, we will need it.
    next if node["rebar_ohai"]["in_docker"] && IP.coerce("172.17.0.0/16").include?(addr)
    Chef::Log.info("#{nic.name}: Removing #{addr.to_s}")
    nic.remove_address addr
  end if old_iface
  iface["addresses"].reject{|i|nic.addresses.member?(i)}.each do |addr|
    Chef::Log.info("#{nic.name}: Adding #{addr.to_s}")
    nic.add_address addr
  end
  Chef::Log.info("pbr = #{iface['pbr']} router = #{iface['router']}")
  if iface['pbr'] and iface['router']
    v4addrs, v6addrs = iface["addresses"].map{|a|::IP.coerce(a)}.partition{|a|a.v4?}
    v4addr = v4addrs.first.to_s.split('/')[0] rescue nil
    pbr_number = pbrs[iface['pbr']]
    Chef::Log.info("Removing table #{pbr_number} info")
    while ::Kernel.system("ip rule del table #{pbr_number}") do
      # Make sure all the rules gone
    end
    while ::Kernel.system("ip route del table #{pbr_number}") do
      # Make sure all the routes gone
    end
    if v4addr
      Chef::Log.info("Adding table #{pbr_number} info #{v4addr} #{nic.name} #{iface['router']}")
      ::Kernel.system("ip rule add to #{v4addr} dev #{nic.name} table #{pbr_number}")
      ::Kernel.system("ip rule add from #{v4addr} to any table #{pbr_number}")
      ::Kernel.system("ip route add default via #{iface['router']} table #{pbr_number} dev #{nic.name} src #{v4addr}")
    end
  end
  # Make sure we are using the proper default route.
  unless default_route.empty?
    if ::Kernel.system("ip route show dev #{nic.name} |grep -q default") &&
        (default_route[:nic] != nic.name)
      Chef::Log.info("Removing default route from #{nic.name}")
      ::Kernel.system("ip route del default dev #{nic.name}")
    elsif default_route[:nic] == nic.name
      ifs[nic.name]["gateway"] = default_route[:gateway]
      unless ::Kernel.system("ip route show dev #{nic.name} |grep -q default")
        Chef::Log.info("Adding default route via #{default_route[:gateway]} to #{nic.name}")
        ::Kernel.system("ip route add default via #{default_route[:gateway]} dev #{nic.name}")
      end
    end
  end
end

if ["delete","reset"].member?(node["state"])
  # We just had the rug pulled out from under us.
  # Do our darndest to get an IP address we can use.
  Nic.refresh_all
  Nic.nics.each{|n|
    next if n.name =~ /^lo/
    n.up
    break if ::Kernel.system("dhclient -1 #{n.name}")
  }
end

# Wait for the networks to come back
node["rebar"]["network"].each do |netname,net|
  next if net["conduit"] == "bmc"
  unless net["targets"]
    Chef::Log.info("Network #{netname} does not have any targets to ping.")
    next
  end
  reachable = false
  src_4, src_6 = net["addresses"].map{|a|IP.coerce(a)}.partition{|a|a.v4?}
  tgt_4, tgt_6 = net["targets"].map{|a|IP.coerce(a)}.partition{|a|a.v4?}
  # Figure out what address to try and ping.
  target = nil
  if !tgt_6.empty? && !src_6.empty?
    # If our target and us nave an ipv6 address, try that.
    target = tgt_6.first
  elsif !tgt_4.empty? && !src_4.empty?
    # Otherwise, try ipv4 if we both have one of those.
    target = tgt_4.first
  end
  # We do not have compatible connectivity.
  unless target
    Chef::Log.info("No target addresses are in the same address family.")
    next
  end
  60.times do
    if target.reachable?
      reachable = true
      break
    end
    sleep 1
  end
  if reachable
    Chef::Log.info("Network #{netname} is alive.")
  else
    Chef::Log.error("Network #{netname} is not alive.")
  end
end

node.set["rebar_wall"] ||= Mash.new
node.set["rebar_wall"]["network"] ||= Mash.new
saved_ifs = Mash.new
ifs.each {|k,v|
  addrs = v["addresses"].map{|a|a.to_s}.sort
  saved_ifs[k]=v
  saved_ifs[k]["addresses"] = addrs
}
Chef::Log.info("Saving interfaces to rebar_wall: #{saved_ifs.inspect}")


Chef::Log.info("Final nic configuration:")
Chef::Log.info(%x{ip addr show})
Chef::Log.info(%x{ip -6 addr show})
Chef::Log.info(%x{ip link show})
node.set["rebar_wall"]["network"]["interfaces"] = saved_ifs
node.set["rebar_wall"]["network"]["nets"] = if_mapping

template "/etc/iproute2/rt_tables" do
  source "rt_tables.erb"
  owner "root"
  group "root"
  variables({
              :pbrs => pbrs
            })
end

case node["platform"]
when "debian","ubuntu"
  template "/etc/network/interfaces" do
    source "interfaces.erb"
    owner "root"
    group "root"
    variables({ :interfaces => ifs })
  end
when "centos","redhat","fedora"
  # add redhat-specific code here
  Nic.nics.each do |nic|
    next unless ifs[nic.name]
    template "/etc/sysconfig/network-scripts/ifcfg-#{nic.name}" do
      source "redhat-cfg.erb"
      owner "root"
      group "root"
      variables({
                  :interfaces => ifs, # the array of config values
                  :nic => nic # the live object representing the current nic.
                })
    end
    template "/etc/sysconfig/network-scripts/route-#{nic.name}" do
      source "redhat-route-cfg.erb"
      owner "root"
      group "root"
      variables({
                  :interfaces => ifs, # the array of config values
                  :nic => nic # the live object representing the current nic.
                })
    end
    template "/etc/sysconfig/network-scripts/rule-#{nic.name}" do
      source "redhat-rule-cfg.erb"
      owner "root"
      group "root"
      variables({
                  :interfaces => ifs, # the array of config values
                  :nic => nic # the live object representing the current nic.
                })
    end
  end
when "suse"
  Nic.nics.each do |nic|
    next unless ifs[nic.name]
    template "/etc/sysconfig/network/ifcfg-#{nic.name}" do
      source "suse-cfg.erb"
      variables({
                  :interfaces => ifs,
                  :nic => nic
                })
    end
    template "/etc/sysconfig/network/ifroute-#{nic.name}" do
      source "suse-route.erb"
      variables({
                  :interfaces => ifs,
                  :nic => nic
                })
    end if ifs[nic.name]["gateway"]
    ## TODO: PBR Work
  end
end
