#!/c/Ruby187/bin/ruby
# Copyright (c) 2013 Dell Inc.
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

require File.join(File.dirname(__FILE__), 'raid_data')

class Crowbar
class RAID
class LSI_MegaCli < Crowbar::RAID::Driver

  attr_accessor :controllers, :debug

  def initialize(node)
  end

  CMD = '/opt/MegaRAID/MegaCli/MegaCli64'
  @@vol_re = /Virtual Drive:\s*(\d+)\s*\(Target Id:\s*(\d+)\)/
  @@disk_re = /PD:\s*(\d+)\s*Information/
  @@adapter_line = /^Adapter #(\d+)$/
  RAID_MAP = {
    :RAID0 => "-r0",
    :RAID1 => "-r1",
    :RAID5 => "-r5",
    :RAID6 => "-r6",
    :RAID00 => "-r00",
    :RAID10 => "-r10",
    :RAID50 => "-r50",
    :RAID60 => "-r60"
  }
  INV_CAP_MAP = {
    "RAID0" => :RAID0,
    "RAID1" => :RAID1,
    "RAID5" => :RAID5,
    "RAID6" => :RAID6,
    "RAID00" => :RAID00,
    "RAID10" => :RAID10,
    "RAID50" => :RAID50,
    "RAID60" => :RAID60
  }

  def enumerate_topology
    @controllers = []
    ## adpCount sets the return code to 0 if no controller is found
    begin
      run_tool(nil, 0, ["-adpCount"])
    rescue Exception => e
      puts "Unable to enumerate controller via MegaCLI...#{e.message}"
      return []
    end

    adapter_lines = {}
    current_array = nil
    run_tool(0, nil, ["-AdpAllinfo", "-aAll"]) do |f|
      f.readlines.each do |line|
        if @@adapter_line.match(line)
          current_array = []
          adapter_lines[$1] = current_array
        else
          current_array << line if current_array
        end
      end
    end

    adapter_lines.keys.each do |cntl|
      c = parse_cntl_info(cntl, adapter_lines[cntl])
      phys_disks = run_tool(0, nil, ["-PDlist", "-a#{cntl}"])
      c.disks = parse_dev_info(phys_disks, c)
      vols = run_tool(0, nil, ["-ldpdinfo", "-a#{cntl}"])
      c.volumes = parse_volumes(vols, c)
      fix_up_jbod(c)
      @controllers << c
    end

    @controllers
  end

  def describe
    "MegaCLi driver"
  end

  def create_vd(controller, volume)
    cid = controller.controller_id
    type = volume[:type]
    name = volume[:name]
    max_size = volume[:size]
    max_size = nil if max_size == "MAX"
    disk_ids = []
    volume[:disks].each { |d| disk_ids << "#{d.enclosure}:#{d.slot}" }

    if volume[:stripe_size]
      stripe_size = "-strpsz#{volume[:stripe_size]}"
    end

    ## build up the command...
    text = ""
    cmd = []
    case type
      when :RAID0,:RAID1,:RAID5,:RAID6
        cmd << "-CfgLdAdd"
        array_type = RAID_MAP[type]
        cmd << "#{array_type}[ #{disk_ids.join(',')} ]"
        # size is specified in megabytes
        cmd << "-sz#{max_size/MEGA}" if max_size
        cmd << stripe_size if stripe_size

      when :RAID00,:RAID10,:RAID50,:RAID60
        cmd << "-CfgSpanAdd"
        # GREG: This is hard coded for RAID10
        # GREG: need to figure out the striping for others.
        cmd << RAID_MAP[type]
        disk_cnt = disk_ids.length
        disk_cnt = disk_cnt -1 if disk_cnt % 2 >0  # can't use odd #.
        span_cnt = disk_cnt/2 
        (1..span_cnt).each { |x|
          cmd <<  "-array#{x}[#{disk_ids[0]}, #{disk_ids[1]} ]"
          disk_ids.shift
          disk_ids.shift
        }

        # size is specified in megabytes
        cmd << "-sz#{max_size/MEGA}" if max_size
        cmd << stripe_size if stripe_size

      when :JBOD
        # will create a separate volume for each drive
        cmd << "-CfgEachDskRaid0"

      else
        raise "unknown raid level requested: #{type}"
      end

    cmd << ["-a#{cid}"]
    run_tool(0, nil, cmd)
  rescue
    log("create returned: #{text}", :ERROR)
    raise
  end

  def delete_vd(volume)
    if volume.raid_level == :JBOD
      # Clear each volume
      volume.members.each do |d|
        next unless d.vol_id
        text = ""
        run_tool(0, nil, ["-CfgLdDel", "-L#{d.vol_id}", "-a#{volume.controller.controller_id}"]) 
      end
    else
      text = ""
      run_tool(0, nil, ["-CfgLdDel", "-L#{volume.vol_id}", "-a#{volume.controller.controller_id}"]) 
    end
  rescue
    log("delete returned: #{text}", :ERROR)
    raise 
  end
      
  # Nic first is ignored
  def set_boot(controller, volume, nic_first = true)
    #### set a bood drive. 
    ## - If there is any kind of volume info, pick first volume
    begin
      if volume and volume.raid_level != :JBOD
        bootVol = volume.vol_id          
        puts "Will use boot volume:#{bootVol}"                    
        run_tool(0, nil, ["-adpBootDrive", "-set", "-l#{bootVol}", "-a#{controller.controller_id}"]) if (bootVol)
      elsif volume and volume.raid_level == :JBOD
        ## JBOD is made up of RAID0 members...volume IDs don't appear sorted
        ## Adding code to sort and pick lowest volume id as bootable drive
        sorted_ids = []
        volume_ids = [] 
        bootVol    = ""
        if ((volume.members) and (!volume.members.empty?))
          bootVol = volume.members[0].vol_id if (volume.members[0].vol_id)
          puts "Set boot volume id to #{bootVol}"
          volume.members.each do |vol|
            volume_ids << vol.vol_id
          end
        end
        sorted_ids = volume_ids.sort! if ((volume_ids) and (!volume_ids.empty?))
        bootVol = sorted_ids[0] if ((sorted_ids) and (!sorted_ids.empty?)) 
        puts "Will use boot jbod fake vol id :#{bootVol}"                    
        run_tool(0, nil, ["-adpBootDrive", "-set", "-l#{boot}", "-a#{controller.controller_id}"]) if (bootVol.length > 0)
      elsif !controller.disks.empty?
        d = controller.disks[0]
        boot = "#{d.enclosure}:#{d.slot}" if ((d.enclosure) and (d.slot))
        puts "Will use boot disk: #{boot}"
        run_tool(0, nil, ["-adpBootDrive", "-set", "-physdrv[#{boot}]", "-a#{controller.controller_id}"]) if (boot)
      else
        log("not changing boot drive.. not enough info")
      end
    rescue Exception => e
      puts "Exception caught in set_boot...#{e.message}"
    end
  end

  def clear_controller_config(controller, type)
    if type == :foreign
      run_tool(0, nil, ["-CfgForeign", "-Clear", "-a#{controller.controller_id}"])
    else
      run_tool(0, nil, ["-CfgClr", "-a#{controller.controller_id}"])
    end
  end

  #
  # All unallocated disks are in the jbod volume
  #
  def fix_up_jbod(c)
    jbod_disks = []

    # Raid 0 with a single Drive is JBOD.
    # Remove from volumes and add to list
    del = []
    c.volumes.each do |v|
      del << v if v.raid_level == :RAID0 and v.members.length == 1
    end
    del.each do |v| 
      # Save off the vol id on the disk
      v.members[0].vol_id = v.vol_id
      c.volumes.delete(v)
      jbod_disks << v.members[0]
    end

    unless jbod_disks.empty?
      rv = Crowbar::RAID::Volume.new
      rv.controller = c
      rv.raid_level = :JBOD
      rv.members = jbod_disks
      c.volumes << rv
    end
  end

=begin
  Parse a controller specific set of lines from -AdpAllInfo -aAll
  There is a wealth of info ignored because the other controllers
  don't have it.  One day....
=end     
  CNTR_KEY_MAP = {
    "Product Name" => :product_name,
    "Device Id" => :device_id,
    "SubDeviceId" => :sub_device_id,
    "Vendor Id" => :vendor_id,
    "SubVendorId" => :sub_vendor_id,
    "FW Version" => :firmware_version,
    "RAID Level Supported" => :supported_raid_levels,
# Optional
    "BIOS version" => :bios_version,
    "Channel description" => :channel,
    "Slot" => :slot,
    "Segment" => :segment,
    "Initiator ID" => :initiator_id,
    "Maximum physical devices" => :max_phys_drives,
    "Concurrent commands supported" => :concurrent_commands,
    "FW Package Build" => :firmware_package,
    "Serial No" => :serial_number
  }
  CNTR_PCI_MAP = {
    "Bus Number" => :bus,
    "Device Number" => :device,
    "Function Number" => :function
  }
  def parse_cntl_info(cid, lines)
    c = Controller.new(:controller_id => cid)
    c.driver = self

    begin
      key, value = extract_value(lines.shift)
      next if !key or key == ""
      new_key = CNTR_KEY_MAP[key]
      if new_key
        case new_key
          when :supported_raid_levels
            rl = []
            value.split(",").each do |r|
              tmp = INV_CAP_MAP[r.strip]
              rl << tmp if tmp
            end
            value = rl
            c.set(:raid_capable, true)
        end
        c.set(new_key, value)
      end
      log("MEGACLI Controller Unknown key: #{key} #{value}", :WARN) unless new_key
    end while (!lines.empty?)

        ## we know how to create jbod, by faking it ;)
    levels = c.supported_raid_levels
    c.supported_raid_levels << :JBOD unless c.supported_raid_levels.count(:JBOD) >0  


    # Also /opt/MegaRAID/MegaCli/MegaCli64 -AdpGetPciInfo -a#{cid}
    lines = run_tool(0, nil, ["-AdpGetPciInfo", "-a#{cid}"])
    begin
      key, value = extract_value(lines.shift)
      next if !key or key == ""
      new_key = CNTR_PCI_MAP[key]
      if new_key
        case new_key
          when :bus, :function, :device
            value = value.to_i
        end
        c.set(new_key, value)
      end
      log("MEGACLI Controller PCI Unknown key: #{key} #{value}", :WARN) unless new_key
    end while (!lines.empty?)
    c
  end

=begin
 On some systems, enclosure info is not avail and they report N/A 
 On those systems, drives should be identified using an empty string 
 for enclosure
=end
def clean_enclosure(s)
  s = '' if s == 'N/A' #handle stupid 2100's enclosures.
  s 
end


=begin
 Parse information about available pyhsical devices.
  The method expects to parse the output of:
  MegaCli64 -PDlist -aAll
=end     
  DISK_KEY_MAP = {
    "Raw Size" => :size,
    "Enclosure Device ID" => :enclosure,
    "Slot Number" => :slot,
    "Inquiry Data" => :model,
    "SAS Address(0)" => :sas_address,
    "Firmware state" => :status,
    "PD Type" => :protocol,
    "Media Type" => :media_type,
  }
  def parse_dev_info(lines, controller)
    devs = []

    rd = nil
    begin
      line = lines.shift

      if line =~ /Enclosure Device ID:/
        key, enclosure = extract_value line
        rd = Crowbar::RAID::RaidDisk.new                       
        rd.controller = controller
        devs << rd
        rd.enclosure = enclosure
      end
      next unless rd

      key, value = extract_value line
      next if !key or key == ""
      new_key = DISK_KEY_MAP[key]
      if new_key
        case new_key
        when :enclosure
          value = clean_enclosure(value)
          when :size
            # sector count * 512 byte sector          
            value = Integer(/.*\[(.*) .*\]/.match(value)[1]) * 512 
          when :model
            arr = value.split(" ")
            rd.set(:manufacturer, arr[0].strip) if arr[0]
            value = arr[1].strip if arr[1]
            rd.set(:serial_number, arr[2].strip) if arr[2]
          when :media_type
            value = :DISK if value == "Hard Disk Device"
            value = :SSD if value == "Solid State Device"
          when :protocol
            value = :SATA if value == "SATA"
            value = :SAS if value == "SAS"
        end
        rd.set(new_key, value)
      end
      log("MEGACLI Disk Unknown key: #{key} #{value}", :WARN) unless new_key
    end while lines.length > 0
    return devs
  end
  
=begin
  Break the output into "stanzas"- one for each volume, and 
  pass the buck down to the volume parsing method.     

  The method expects the output of:
  MegaCli64 -ldpdinfo -aAll
=end
  def parse_volumes(lines, controller)
    vols = []    
    txt = save = ""
    # find first volume
    skip_to_find lines, @@vol_re
    begin
      # find the next one, to "bracket"the volume
      save = lines.shift if lines.length > 0 
      txt = skip_to_find lines, @@vol_re
      next if txt.length ==0  # no more info          
      vols << parse_vol_info([ save ] + txt, controller)
    end while txt.length > 0   
    # the main RAID controller uses a JBOD "volume" to represent jbod
    # create a fake volume to capture the ID of the first drive (So we can)

    vols
  end
  
=begin
 Parse info about one volume
=end  
  VOL_KEY_MAP = {
    "Name" => :vol_name,
    "State" => :status,
    "RAID Level" => :raid_level,
    "Size" => :size,
    "Strip Size" => :strip_size,
    "Number Of Drives per span" => :span_length,
    "Span Depth" => :span_depth
  }
  def parse_vol_info(lines, controller)
    skip_to_find lines, @@vol_re
    return if lines.length == 0 and log("no more info") 

    rv = Crowbar::RAID::Volume.new
    rv.controller = controller

    # Parse volume info
    begin
      line = lines.shift

      break if line =~ /PD: /  ## stop when we get to the first phys drive
      if line =~ @@vol_re
        rv.vol_id, rv.vol_name = @@vol_re.match(line)[1,2]
        next
      end

      key, value = extract_value line
      new_key = VOL_KEY_MAP[key]
      if new_key
        case new_key
          when :vol_name
            value = rv.vol_name unless value
          when :span_depth
            value = value.to_i
            if value > 1 and rv.raid_level
              case rv.raid_level
                when :RAID0
                  rv.raid_level = :RAID00
                when :RAID1
                  rv.raid_level = :RAID10
                when :RAID5
                  rv.raid_level = :RAID50
                when :RAID6
                  rv.raid_level = :RAID60
              end
            end
          when :strip_size
            value = size_to_bytes(value.strip) / KILO
          when :span_length
            value = value.to_i
          when :size
            value = size_to_bytes(value.strip)
          when :raid_level
            case value
              when /Primary-0, Secondary-0/
                value = :RAID0
              when /Primary-1, Secondary-0/ 
                value = (rv.span_depth and rv.span_depth > 1) ? :RAID10 : :RAID1
              when /Primary-5, Secondary-0/ 
                value = (rv.span_depth and rv.span_depth > 1) ? :RAID50 : :RAID5
              when /Primary-6, Secondary-0/ 
                value = (rv.span_depth and rv.span_depth > 1) ? :RAID60 : :RAID6
            end
        end
        rv.set(new_key, value)
      end
      log("MEGACLI Volume Unknown key: #{key} #{value}", :WARN) unless new_key
    end while lines.length > 0

    # Parse drives for the volume
    enclosure = nil
    slot = nil
    begin
      line = lines.shift

      enclosure = clean_enclosure($2) if line =~ /Enclosure Device (ID|value): (.*)/      
      slot = $1 if line =~ /Slot Number: (.*)/
      if slot and enclosure
        rd = nil
        controller.disks.each do |d|
          if d.enclosure == enclosure and d.slot == slot
            rd = d
            break
          end
        end
        unless rd
          log("Failed to find disk in physical list", :WARN)
          rd = Crowbar::RAID::RaidDisk.new
          rd.enclosure = enclosure
          rd.slot = slot
          rd.name = "#{enclosure}:#{slot}"
        end
        rv.members << rd

        slot = enclosure = nil
      end
    end while lines.length > 0

    log("found volume with #{rv.members.length} drives and #{rv.span_depth} spans ")
    rv
  end    
  
  def run_tool(success, error, args, &block)    
    cmd = [CMD, *args]
    cmdline = cmd.join(" ")
    run_command(cmdline, success, error, &block)
  end
end # MEGACLI
end # RAID
end # Crowbar


if __FILE__ == $0
  $in_chef=false
  l = Crowbar::RAID::LSI_MegaCli.new(nil)
  l.debug = true
  c = l.enumerate_topology

  c.each do |con|
    puts "con: Disks(#{con.disks.length}) Volumes(#{con.volumes.length})"
    con.disks.each { |d| puts "  Disk #{d.enclosure}:#{d.slot} = #{d.size}" }
    con.volumes.each { |v| puts "  Volume #{v.vol_name} = #{v.size} #{v.raid_level}" }
  end

#  con = c[0]
#  vhash = { :name => "greg", :size => "MAX", :disks => con.disks, :type => :RAID0 }
#  l.create_vd(con, vhash)
#  l.delete_vd(con.volumes[0])
#  l.set_boot(con, con.volumes[0])
#  l.set_boot(con, nil)
end
