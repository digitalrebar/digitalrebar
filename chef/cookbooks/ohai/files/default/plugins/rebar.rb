# Copyright 2011, Dell
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

# This hackjob is needed for loading the cstruct gem.

provides "rebar_ohai"
rebar_ohai Mash.new
rebar_ohai[:in_docker] = File.executable?("/.dockerinit")

Gem.clear_paths
outer_paths=%x{gem env gempath}.split(':')
outer_paths.each do |p|
  next if Gem.path.member?(p.strip)
  Gem.paths.path << p.strip
end
Chef::Log.debug("Gem path set to #{Gem.paths.path}")


require 'etc'
require 'pathname'
require 'tempfile'
require 'timeout'

class System
  def self.background_time_command(timeout, background, name, command)
    fd = Tempfile.new("tcpdump-#{name}-")
    fd.chmod(0700)
    fd.puts <<-EOF.gsub(/^\s+/, '')
#!/bin/bash
#{command} &
sleep #{timeout}
kill %1
rm -f #{fd.path}
EOF
    fd.close

    if background
      system(fd.path + " &")
    else
      system(fd.path)
    end
  end
end

mac_map = {}
rebar_ohai[:detected] = Mash.new unless rebar_ohai[:detected]
rebar_ohai[:detected][:network] = Mash.new unless rebar_ohai[:detected][:network]
Dir.foreach('/sys/class/net') do |entry|
  next if entry =~ /\./

  # We only care about actual physical devices.
  # or eth devices in containers
  next unless File.exists? "/sys/class/net/#{entry}/device" or entry =~ /eth/
  #Chef::Log.debug("examining network interface: " + entry)

  type = File::open("/sys/class/net/#{entry}/type") do |f|
    f.readline.strip
  end rescue '0'
  #Chef::Log.debug("#{entry} is type #{type}")
  next unless type == '1'

  s1 = File.readlink("/sys/class/net/#{entry}") rescue ""
  spath = File.readlink("/sys/class/net/#{entry}/device") rescue 'Unknown'
  spath = s1 if s1 =~ /pci/
  spath = spath.gsub(/.*pci/, '').gsub(/\/net\/.*/, '')
  #Chef::Log.debug("#{entry} spath is #{spath}")

  mac_addr = File.open("/sys/class/net/#{entry}/address", "r") do |f|
    mac_map[entry] = f.gets().strip
  end rescue ''

  ips = %x{ip addr show #{entry} | grep inet | awk '{ print $2 }'}.split("\n") rescue []

  speed = File::open("/sys/class/net/#{entry}/speed") do |f|
    f.readline.strip
  end rescue '0'
  speeds = [speed]
  rebar_ohai[:detected][:network][entry] = { :mac => mac_addr, :path => spath, :speeds => speeds, :ips => ips }

  #Chef::Log.debug("MAC is #{mac_addr.strip}")
  rebar_ohai[:detected][:mac_map] = mac_map
end

rebar_ohai[:disks] ||= Mash.new
disk_ordering = Hash.new
Dir.foreach("/sys/block")do |device|
  path = File.join("/sys/block",device)
  next unless File.symlink?(path)
  link = File.readlink(path)
  linkparts = link.split('/')
  linkparts.shift
  # Ignore floppy disks and virtual devices
  next if linkparts[1] =~ /virtual|floppy/
  # Ignore removable devices
  next unless IO.read(File.join(path,"removable")).strip == "0"
  # Ignore readonly devices
  next unless IO.read(File.join(path,"ro")).strip == "0"
  # Only care about things hanging off a PCI bus.
  next unless link =~ /\/pci.*\/block\//
  # OK, we have something we care about.
  # Save some information needed to calculate our global overall order.
  abstract_addr = Hash.new
  linkparts.each do |lp|
    case
    when lp =~ /^[0-9a-f]{4}:[0-9a-f]{2}:[0-9a-f]{2}\.[0-9a-f]$/
      abstract_addr[:pci] ||= []
      abstract_addr[:pci] << lp.split(/[:.]/).map{|m|m.to_i(16)}
    when lp =~ /^([0-9]:){3}[0-9]$/
      abstract_addr[:scsi] ||= []
      abstract_addr[:scsi] << lp.split(":").map{|m|m.to_i(16)}
    end
  end
  abstract_addr[:sysfslink] = link
  disk_ordering[abstract_addr]=device

  # Find the appropriate by-id and by-path symlinks.
  # This will hopefully be pretty stable.
  ["by-id", "by-path"].each do |path|
    Dir.glob("/dev/disk/#{path}/*").each do |id|
      next unless File.symlink?(id)
      next unless File.readlink(id).split('/')[-1] == device
      rebar_ohai[:disks][path] ||= Mash.new
      rebar_ohai[:disks][path][id.split('/')[-1]] = device
      rebar_ohai[:disks][device] ||= Mash.new
      rebar_ohai[:disks][device][path] ||= Array.new
      rebar_ohai[:disks][device][path] << id.split('/')[-1]
    end
  end
  rebar_ohai[:disks][device] ||= Mash.new
  # Disks are available if they have no holders, slaves, or partitions,
  # and blkid does not return anything for them.
  if Dir.glob(File.join(path,"holders/*")).empty? &&
      Dir.glob(File.join(path,"slaves/*")).empty? &&
      Dir.glob(File.join(path,"#{device}*")).empty? &&
      %x{blkid -o value -s TYPE /dev/#{device}}.strip.empty?
    rebar_ohai[:disks][device][:available] = true
  else
    rebar_ohai[:disks][device][:available] = false
  end
  rebar_ohai[:disks][device][:usb] = !!(link =~ /\/usb/)
  preferred = nil
  if rebar_ohai[:disks][device]["by-id"]
    [ /^scsi-[a-zA-Z]/,
      /^scsi-/,
      /^ata-/,
      /^cciss-/ ].each do |finder|
      preferred = rebar_ohai[:disks][device]["by-id"].find{|b| b =~ finder}
      next unless preferred
      preferred = "/dev/disk/by_id/#{preferred}"
      break
    end
    preferred ||= "/dev/disk/by_id/#{rebar_ohai[:disks][device]["by-id"].first}"
  elsif rebar_ohai[:disks][device]["by-path"]
    preferred = "/dev/disk/by_id/#{rebar_ohai[:disks][device]["by-path"].first}"
  else
    preferred = "/dev/#{device}"
  end
  rebar_ohai[:disks][device][:preferred_device_name] = preferred
end
rebar_ohai[:disks][:order] = Array.new
disk_ordering.keys.sort{|a,b|
  res = 0
  if a[:pci] && b[:pci]
    res = a[:pci] <=> b[:pci]
  end
  if res == 0 && a[:scsi] && b[:scsi]
    res = a[:scsi] <=> b[:scsi]
  end
  res = a[:sysfslink] <=> b[:sysfslink] if res == 0
  res
}.each do |k|
  rebar_ohai[:disks][:order] << disk_ordering[k]
end
