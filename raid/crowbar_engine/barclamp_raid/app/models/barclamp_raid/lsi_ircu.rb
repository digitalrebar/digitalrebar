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

# WARNING: This module still needs conversion to comply with the new
#          RAID driver superclass! It is stubbed out to be nonfunctional.
module BarclampRaid
  class Lsi_Sasircu < BarclampRaid::Driver

    MAX_RAID10_DISKS = 10

    def useable?
      false
      run_tool(0, nil, ["list"]).each do |line|
        next unless (line =~ /:/) and !(line =~ /SAS2IRCU/)
        answer << line.split(" ")
      end
    end

    def enumerate_topology
      @controllers = []
      begin
        text = []
        # Get a list: returns an array of lists
        # list contains:
        # [id, type, vendor, device, PCI, SubVen, SubDev]
        # e.g.
        # [["0", "SAS2008", "1000h", "72h", "00h:03h:00h:00h", "1170h", "6019h"]]
        answer = []

        # Gather bonus info for each controller found
        answer.each do |raw_cntr|
          lines = run_tool(0, nil, [raw_cntr[0], "display"])

          cntr_info = find_stanza lines,"Controller information"
          phyz = find_stanza lines,"Physical device information"
          logical = find_stanza lines, "IR Volume information"

          c = parse_cntr_info cntr_info, raw_cntr
          if (c)
            c.disks = parse_dev_info phyz, c
            c.volumes = parse_vol_info logical, c

            fix_up_jbod(c)

            c.driver = self

            @controllers << c
          end
        end
      rescue Exception => e
        puts "Failed enumeration of controller/devices on system using sas2ircu...#{e.message}"
      end

      @controllers
    end

    #       Issue the command to create a RAID volue:
    #         The format of the CREATE command is
    #       sas2ircu <controller #> create <volume type> <size>
    #       <Encl:Bay> [Volume Name] [noprompt]
    #       where <controller #> is:
    #       A controller number between 0 and 255.
    #         where <volume type> is:
    #         The type of the volume to create and is either RAID1 (or)
    #       RAID1E (or) RAID0 (or) RAID10.
    #         where <size> is:
    #         The size of the volume to create. It should be given in Mbytes
    #       e.g. 2048 or 'MAX' to use the maximum size possible.
    #         where <Encl:Bay> is:
    #         A list of Encl:Bay pairs identifying the disk drives you
    #       wish to include in the volume being created. If the volume type is
    #       'RAID1', the first drive will be selected as the primary and the
    #       second as the secondary drive.
    #         For a type 'RAID1' volume exactly 2 disks must be specified.
    #         For a type 'RAID1E' volume min of 3 disks must be specified.
    #         For a type 'RAID0' volume min of 2 disks must be specified.
    #         For a type 'RAID10' volume min of 4 disks must be specified.
    #         where [Volume Name] is an optional argument that can be used
    #       to identify a Volume with a user specified Alpha-numeric string
    #       where noprompt is an optional argument that eliminates
    #       warnings and prompts
    
    def create_vd(controller, volume)
      ## build up the command...
      max_size       = (Crowbar::RAID::TERA * 2 - Crowbar::RAID::MEGA) / Crowbar::RAID::MEGA
      size           = nil
      curr_boot_mode = nil
      pend_boot_mode = nil
      vol_type       = INV_MAP[volume[:type]]
      name           = ""
      name           = volume[:name] if volume[:name]
      cid            = controller.controller_id
      no_boot_mode   = false

      if (@nodeinfo["crowbar_wall"] and @nodeinfo["crowbar_wall"]["track"] and @nodeinfo["crowbar_wall"]["track"]["bios_settings"] and @nodeinfo["crowbar_wall"]["track"]["bios_settings"]["boot_mode"])
        boot_mode_data = @nodeinfo["crowbar_wall"]["track"]["bios_settings"]["boot_mode"]
        curr_boot_mode = boot_mode_data["current"].to_s if (boot_mode_data)
        pend_boot_mode = boot_mode_data["desired"].to_s if (boot_mode_data)

        puts "Current boot mode  = #{curr_boot_mode}"
        puts "Pending boot mode  = #{pend_boot_mode}"
      else
        puts "Unable to determine boot mode from CB wall..defaulting to BIOS mode"
        no_boot_mode = true
      end

      log("create_vd: ircu doesn't support setting stripe size", :ERROR) if volume[:stripe_size]

      ## Limit the number of disks being used if it's a RAID 10 array
      if (volume[:type] == :RAID10 and volume[:disks].length > MAX_RAID10_DISKS)
        numDisks = volume[:disks].length - 1
        if (numDisks and numDisks > MAX_RAID10_DISKS)
          volume[:disks].slice!(MAX_RAID10_DISKS, numDisks)
        end
      end

      disk_ids = []
      volume[:disks].each do |d|
        disk_ids << "#{d.enclosure}:#{d.slot}"
      end

      if (volume[:type] == :JBOD or !vol_type)
        log("Not creating JBODs using sas2ircu...Exiting")
        return
      end

      size = get_array_size(volume) if (!volume[:size] or volume[:size] == "MAX")

      if ( (pend_boot_mode and pend_boot_mode == "bios") or (curr_boot_mode and curr_boot_mode == "bios") or (no_boot_mode == true))
        if (!size)
          size = "MAX"
        else
          size = max_size if (max_size < size)
        end
      end

      puts "Creating VD with size of #{size}"

      text = ""
      run_tool(0, nil, [cid, "create", vol_type, size, disk_ids.join(" "), "'#{name}'", "noprompt"]){ |f|
        text = f.readlines
      }
      text.to_s.strip
    rescue
      log("create returned: #{text}", :ERROR)
      raise
    end

    def delete_vd(volume)
      text = ""
      cid = volume.controller.controller_id
      id = volume.vol_id
      if (id)
        run_tool(0, nil, [cid, "deletevolume", id, "noprompt"]) { |f|
          text = f.readlines
        }
      else
        log("No volume id determined....Nothing to delete")
      end
    rescue
      log("delete returned: #{text}", :ERROR)
      raise
    end

    # nic_first is ignored here.
    def set_boot(controller, volume, nic_first = true)
      ## set the boot volume or drive.
      ## if we have a volume, otherwise, first drive.
      begin
        if volume and volume.raid_level != :JBOD
          bootVol = volume.vol_id
          log("Will use boot volume:#{bootVol}")
          puts "Will use boot volume:#{bootVol}"
          run_tool(0, nil, [controller.controller_id, "bootir", "#{bootVol}"]) if (bootVol)
        elsif !controller.disks.empty? and controller.raid_capable
          d = controller.disks[0]
          d = volume.members[0] if volume and !volume.members.empty?
          boot = "#{d.enclosure}:#{d.slot}" if ((d.enclosure) and (d.slot))
          log("Will use boot disk: #{boot}")
          puts "Will use boot disk: #{boot}"
          run_tool(0, nil, [controller.controller_id, "bootencl", boot]) if (boot)
        else
        end
      rescue Exception => e
        puts "Caught exception in set_boot..#{e.message}"
      end
    end

    def clear_controller_config(controller, type)
      return true if type == :foreign
      run_tool(0, nil, [controller.controller_id, "delete", "noprompt"])
    end

    #
    # All unallocated disks are in the jbod volume
    #
    def fix_up_jbod(c)
      disks = c.disks.dup

      c.volumes.each do |v|
        disks = disks - v.members
      end

      unless disks.empty?
        rv = Crowbar::RAID::Volume.new
        rv.controller = c
        rv.raid_level = :JBOD
        rv.members = disks
        c.volumes << rv
      end
    end

    CNTR_KEY_MAP = {
      "Controller type" => :product_name,
      "Bus" => :bus,
      "Device" => :device,
      "Function" => :function,
      "Firmware version" => :firmware_version,
      "BIOS version" => :bios_version,
      "Channel description" => :channel,
      "Initiator ID" => :initiator_id,
      "Maximum physical devices" => :max_phys_drives,
      "Concurrent commands supported" => :concurrent_commands,
      "Slot" => :slot,
      "Segment" => :segment,
      "RAID Support" => :raid_capable
    }

    def parse_cntr_info(lines, raw_cntr)
      c = Controller.new(:controller_id => raw_cntr[0],
                         :vendor_id => raw_cntr[2],
                         :sub_vendor_id => raw_cntr[5],
                         :device_id => raw_cntr[3],
                         :sub_device_id => raw_cntr[6],
                         :device_type => raw_cntr[1],
                         :pci_address => raw_cntr[4])

      c.set(:supported_raid_levels, INV_MAP.keys << :JBOD)

      begin
        key, value = extract_value(lines.shift)
        new_key = CNTR_KEY_MAP[key]
        if new_key
          case new_key
          when :bus, :function, :device
            value = value.to_i
          when :raid_capable
            value = value.casecmp("YES") == 0
            c.set(:supported_raid_levels, [:JBOD]) unless value
          end
          c.set(new_key, value)
        end
        log("IRCU Controller Unknown key: #{key} #{value}", :WARN) unless new_key
      end while (!lines.empty?)
      c
    end

    VOL_KEY_MAP = {
      "Volume ID" => :vol_id,
      "Volume Name" => :vol_name,
      "Status of volume" => :status,
      "RAID level" => :raid_level,
      "Size (in MB)" => :size,
      "Volume wwid" => :vol_wwid
    }

    def parse_vol_info(lines, controller)
      vols= []

      rv = nil
      begin
        line = lines.shift

        if line =~ /^IR volume (\d+)\s*/
          rv = Crowbar::RAID::Volume.new
          rv.controller = controller
          vols << rv
        end
        next unless rv

        if line =~ /\s+Physical hard disks\s*:\s*$/
          # Skipping
        elsif line =~ /\s+PHY.* : (\d+):(\d+)\s*$/
          enclosure = $1
          slot = $2
          name = "#{$1}:#{$2}"

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
            rd.name = name
          end
          rv.members << rd
        else
          key, value = extract_value(line)

          new_key = VOL_KEY_MAP[key]
          if new_key
            case new_key
            when :size
              value = value.to_i * MEGA
            when :raid_level
              value = RAID_MAP[value]
            end

            rv.set(new_key, value)
          else
            log("IRCU Volume Unknown key: #{key} #{value}", :WARN)
          end
        end
      end while lines.length > 0
      vols
    end

    DISK_KEY_MAP = {
      "Size (in MB)/(in sectors)" => :size,
      "Enclosure #" => :enclosure,
      "Slot #" => :slot,
      "Model Number" => :model,
      "Manufacturer" => :manufacturer,
      "Serial No" => :serial_number,
      "SAS Address" => :sas_address,
      "State" => :status,
      "Protocol" => :protocol,
      "Drive Type" => :media_type,
      "Firmware Revision" => :firmware_revision,
      "GUID" => :guid
    }

    def parse_dev_info(lines, controller)
      disks = []
      rd = nil
      begin
        line = lines.shift

        if line =~ /^Device is a (.*)$/
          if $1 == "Hard disk"
            rd = Crowbar::RAID::RaidDisk.new
            rd.controller = controller
            disks << rd
          else
            rd = nil
          end
        end
        next unless rd

        key, value = extract_value(line)

        new_key = DISK_KEY_MAP[key]
        if new_key
          case new_key
          when :size
            value = Integer(value.split("/")[1])*512 # use sectors
          when :media_type
            value = :DISK if value =~ /HDD/
            value = :SSD if value =~ /SSD/
          when :protocol
            value = :SATA if value == "SATA"
            value = :SAS if value == "SAS"
          end

          rd.set(new_key, value)
        else
          log("IRCU Disk Unknown key: #{key} #{value}", :WARN)
        end
      end while lines.length > 0
      disks
    end

    ## Rudimentary algorithm to find array size...assumes that ##
    ## all disks are of the same size for v0.1                 ##
    def get_array_size(volume)
      num_disks  = volume[:disks].length
      disk_size  = (volume[:disks][0].size/Crowbar::RAID::MEGA - 2048)  if (volume[:disks] and volume[:disks].length > 0)
      raid_level = volume[:type]
      array_size = nil
      case raid_level
      when :RAID0
        array_size = num_disks * disk_size
      when :RAID1
        array_size = (num_disks / 2 ) * disk_size
      when :RAID10
        array_size = (num_disks / 2 ) * disk_size
      when :RAID5
        array_size = (num_disks - 1 ) * disk_size
      else
        puts "Unknown raid level requested..returning nil"
      end

      puts "RAID level is #{raid_level}"
      puts "Number of disks  #{num_disks}"
      puts "DiskSize is #{disk_size}"
      puts "Array size is #{array_size}"

      return array_size
    end

    #=begin
    #   Output from the LSI util is broken into stanzas delineated with something like:
    #     ------------------------------------------------------------------------
    #     Controller information
    #   ------------------------------------------------------------------------
    #
    #     This method finds a stanza by name and returns an array with its content
    #   =end
    def find_stanza(lines,name)
      lines = lines.dup
      begin
        # find a stanza mark.
        skip_to_find lines,@@re_lines
        lines.shift
        #make sure it's the right one.
      end while lines.length > 0 and  lines[0].strip.casecmp(name) != 0
      lines.shift # skip stanza name and marker
      lines.shift
      log("start of #{name} is #{lines[0]}")

      #lines now starts with the right stanzs.... filter out the rest.
      ours = skip_to_find lines,@@re_lines
      ours
    end

    def run_tool(success, error, args, &block)
      cmd = [CMD]
      cmd = cmd + [*args]
      run_command(cmd.join(" "), success, error, &block)
    end

  end # This driver
end # RAID
