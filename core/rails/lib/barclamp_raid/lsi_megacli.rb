#!/usr/bin/env ruby
# Copyright (c) 2014, Victor Lowther
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

module BarclampRaid
  class Lsi_Megacli < Driver
    @@vol_re = /Virtual Drive:\s*(\d+)\s*\(Target Id:\s*(\d+)\)/
    @@disk_re = /PD:\s*(\d+)\s*Information/
    @@adapter_line = /^Adapter #(\d+)$/

    # Parse a controller specific set of lines from -AdpAllInfo -aAll
    # There is a wealth of info ignored because the other controllers
    # don't have it.  One day....
    CNTR_KEY_MAP = {
      "Product Name" => "product_name",
      "Device Id" => "device_id",
      "SubDeviceId" => "sub_device_id",
      "Vendor Id" => "vendor_id",
      "SubVendorId" => "sub_vendor_id",
      "FW Version" => "firmware_version",
      "RAID Level Supported" => "supported_raid_levels",
      # Optional
      "BIOS version" => "bios_version",
      "Channel description" => "channel",
      "Slot" => "slot",
      "Segment" => "segment",
      "Initiator ID" => "initiator_id",
      "Maximum physical devices" => "max_phys_disks",
      "Concurrent commands supported" => "concurrent_commands",
      "FW Package Build" => "firmware_package",
      "Serial No" => "serial_number",
      "Enable JBOD" => "native_jbod"
    }
    CNTR_PCI_MAP = {
      "Bus Number" => "bus",
      "Device Number" => "device",
      "Function Number" => "function"
    }
    def parse_cntl_info(cid, lines)
      res = Hash.new
      res["id"] = cid
      res["driver"] = self
      res["driver_name"] = @params["name"]
      res["raid_capable"] = false
      lines.each do |line|
        key, value = extract_value(line)
        next if !key or key == ""
        new_key = CNTR_KEY_MAP[key]
        next unless new_key
        res[new_key] = case new_key
                       when "supported_raid_levels"
                         res["raid_capable"] = true
                         @params["raid_levels"] & value.split(',').map{|e|e.downcase.strip}
                       when "native_jbod" then value == "No" ? false : true
                       else
                         value
                       end
      end
      res["supported_raid_levels"] << "jbod" unless res["supported_raid_levels"].include?("jbod")
      # Get the PCI bus info with /opt/MegaRAID/MegaCli/MegaCli64 -AdpGetPciInfo -a#{cid}
      run_tool(0, nil, ["-AdpGetPciInfo", "-a#{cid}"]).each do |line|
        key, value = extract_value(line)
        next if !key or key == ""
        new_key = CNTR_PCI_MAP[key]
        next unless new_key
        res[new_key] = case new_key
                       when "bus", "function", "device"
                       then value.to_i
                       else
                         value
                       end
      end
      res
    end

    def useable?
      begin
        run_tool(nil, nil, ["-adpCount"]).each do |line|
          Rails.logger.warn("BarclampRaid::Lsi_Megacli.useable?: #{line}")
          return true if line =~ /Controller Count:.*([0-9]+)\./ && $~[1].to_i > 0
        end
        return false
      rescue
        return false
      end
    end

    def controllers
      res = []
      adapter_lines = {}
      current_array = nil
      run_tool(0, nil, ["-AdpAllinfo", "-aAll"]).each do |line|
        if @@adapter_line.match(line)
          current_array = []
          adapter_lines[$1] = current_array
        else
          current_array << line if current_array
        end
      end
      adapter_lines.keys.each do |cntl|
        res << BarclampRaid::Controller.new(parse_cntl_info(cntl, adapter_lines[cntl]))
      end
      res
    end

    # On some systems, enclosure info is not avail and they report N/A
    # On those systems, disks should be identified using an empty string
    # for enclosure
    def clean_enclosure(s)
      s == 'N/A' ? '' : s
    end

    # Parse information about available pyhsical devices.
    # The method expects to parse the output of:
    # MegaCli64 -PDlist -aAll
    DISK_KEY_MAP = {
      "Coerced Size" => "disk_size",
      "Slot Number" => "slot",
      "SAS Address(0)" => "sas_address",
      "Firmware state" => "status",
      "PD Type" => "protocol",
      "Media Type" => "media_type",
      "Enclosure Device ID" => "enclosure"
    }

    def parse_disk(controller,lines)
      disk = Hash.new
      disk["controller"]=controller
      lines.each do |line|
        key, value = extract_value line
        next if !key or key == ""
        new_key = DISK_KEY_MAP[key]
        next unless new_key
        disk[new_key] = case new_key
                        when "enclosure" then clean_enclosure(value)
                        when "protocol" then value.downcase.strip
                        when "disk_size"
                          # sector count * 512 byte sector
                          Integer(/.*\[(.*) .*\]/.match(value)[1]) * 512
                        when "media_type"
                          case value
                          when "Hard Disk Device" then "disk"
                          when "Solid State Device" then "ssd"
                          else
                            value
                          end
                        else
                          value
                        end
      end
      BarclampRaid::RaidDisk.new(disk)
    end

    def parse_disks(controller,lines)
      split_lines_on(lines,/Enclosure Device ID:/).map{|d|parse_disk(controller,d)}
    end

    def disks(controller)
      parse_disks(controller,run_tool(0, nil, ["-PDlist", "-a#{controller.id}"]))
    end

    VOL_KEY_MAP = {
      "Name" => "name",
      "State" => "status",
      "RAID Level" => "raid_level",
      "Size" => "vol_size",
      "Strip Size" => "stripe_size",
      # Depending on the volume type, either one could be present.
      "Number Of Drives per span" => "span_length",
      "Number Of Drives" => "span_length",
      "Span Depth" => "spans"
    }

    def parse_volume(controller,lines)
      volume_lines = []
      disk_lines = []
      in_volume = true
      lines.each do |line|
        in_volume = false if line =~ /^PD:/
        if in_volume
          volume_lines << line
        else
          disk_lines << line
        end
      end
      rv = Hash.new
      rv["controller"]=controller
      volume_lines.each do |line|
        if line =~ @@vol_re
          rv["id"], rv["name"] = @@vol_re.match(line)[1,2]
          next
        end
        key, value = extract_value line
        next if !key or key == ""
        new_key = VOL_KEY_MAP[key]
        next unless new_key
        rv[new_key] = case new_key
                      when "spans" then value.to_i
                      when "stripe_size" then BarclampRaid.size_to_bytes(value.strip)
                      when "span_length" then value.to_i
                      when "vol_size" then BarclampRaid.size_to_bytes(value.strip)
                      when "raid_level"
                        md = /Primary-(\d+), Secondary-(\d+)/.match(value)
                        case [md[1],md[2]].map{|m|m.to_i}
                        when [0,0] then "raid0"
                        when [1,0] then "raid1"
                        when [3,0] then "raid3"
                        when [4,0] then "raid4"
                        when [5,0] then "raid5"
                        when [6,0] then "raid6"
                        when [7,0] then "raid7"
                        when [15,0] then "jbod"
                        when [17,0] then "raid1e"
                        when [31,0] then "concat"
                        when [21,0] then "raid5e"
                        when [37,0] then "raid5ee"
                        when [53,0] then "raid5r"
                        when [0,3] then "raid00"
                        when [1,3] then "raid10"
                        when [5,3] then "raid50"
                        when [6,3] then "raid60"
                        else
                          raise "No idea what RAID level #{value} maps to"
                        end
                      else
                        value
                      end
      end
      # Fix up the reported RAID level
      if rv["spans"] && rv["spans"] > 1 &&
          !["raid00","raid10","raid50","raid60"].member?(rv["raid_level"])
        # More than one span, append a zero to the end of the discovered raid level.
        rv["raid_level"] << "0"
      elsif rv["spans"] == 1 && rv["span_length"] == 1
        # only one disk, then it is a jbod.
        rv["raid_level"] = "jbod"
      end
      # Add the disks
      rv["disks"] = parse_disks(controller,disk_lines)
      BarclampRaid::Volume.new(rv)
    end

    def parse_volumes(controller,lines)
      split_lines_on(lines,@@vol_re).map{|v|parse_volume(controller,v)}
    end

    def volumes(controller)
      parse_volumes(controller,run_tool(0, nil, ["-ldpdinfo", "-a#{controller.id}"]))
    end

    def create_vd(controller, volume)
      cid         = controller.id
      raid_level  = volume["raid_level"]
      name        = volume["name"]
      size        = volume["size"] || "max"
      stripe_size = BarclampRaid.size_to_bytes(volume["stripe_size"] || "64 KB")
      candidates = Array.new
      @logger << "Creating #{name} on #{controller.name}\n" if @logger
      if volume["disks"].is_a?(Numeric)
        candidates = controller.find_candidates(volume)
      elsif volume["disks"].is_a?(Array)
        candidates = volume["disks"]
      else
        raise "Don't know how to turn volume['disks'](#{volume["disks"]}) into an array of candidate disks"
      end
      unless candidates.is_a?(Array) &&
          !candidates.empty? &&
          candidates.all?{|c|c.is_a?(BarclampRaid::RaidDisk)}
        raise "Was not passed a satisfiable volume specification to create_vd"
      end
      disks,spans = BarclampRaid.calc_spans(raid_level,candidates.length)
      min_disks, max_disks, _ = BarclampRaid.calc_raid_overhead(raid_level,spans)
      raise "Not enough disks passed to create_vd" unless disks >= min_disks
      raise "Too many disks passed to create_vd" unless disks <= max_disks

      cmd = []
      raid_level = "raid0" if raid_level == "jbod" && !controller.native_jbod
      case raid_level
      when "raid0","raid1","raid5","raid6"
        cmd << "-CfgLdAdd"
        cmd << "-r#{raid_level[-1]}[#{candidates.map{|c|c.id}.join(',')}]"
      when "raid00","raid10","raid50","raid60"
        # Bucketize disks according to the number of spans we want
        disks_per_span = (disks / spans).floor
        arrays = []
        spans.times do
          span = []
          disks_per_span.times do
            span << candidates.shift.id
          end
          arrays << span
        end
        cmd << "-CfgSpanAdd"
        cmd << "-r#{raid_level[-2..-1]}"
        arrays.each_index do |idx|
          cmd << "-array#{idx}[#{arrays[idx].join(',')}]"
        end
      else
        raise "Raid level #{raid_level} Not Implemented"
      end
      cmd << "-strpsz#{stripe_size}"
      if size != "min" && size != "max"
        # Size must be passed in megabytes, and 100 megs seems to be the smallest size.
        size = (BarclampRaid.size_to_bytes(size) / MEGA).ceil
        size = 100 if size < 100
        cmd << "-sz#{size}"
      end
      cmd << ["-a#{cid}"]
      # Create the array, and grab the VD# we created.
      vdline = run_tool(0, nil, cmd).detect{|l|l =~ /Created VD/}
      vol_id = vdline.match(/Created VD (\d+)/)[1]
      # Now, name the volume and disable PD caching.
      ["-Name #{name||"rebar_vol_#{vol_id}"}","DisDskCache",].each do |param|
        cmd = ["-LDSetProp",
               param,
               "-L#{vol_id}",
               "-a#{cid}"]
        lines = run_tool(0, nil, cmd)
        lines.each do |line|
          @logger << line
        end if @logger
      end
      _set_boot(controller, vol_id) if volume["boot"]
      volumes = self.refresh_controller(controller)

      # Return the actual volume per api contract.
      volumes.each do |v|
        return v if v.name == name
      end
    end

    def delete_vd(volume)
      @logger << "Delete VD on #{volume.controller.name} for #{volume.id}\n" if @logger
      lines = run_tool(0, nil, ["-CfgLdDel", "-L#{volume.id}", "-force","-a#{volume.controller.id}"])
      lines.each do |line|
        @logger << line
      end if @logger
      self.refresh_controller(volume.controller)
    end

    def _set_boot(controller, volume_id)
      @logger << "setting boot volume on #{controller.name} for #{volume_id}\n" if @logger
      #### set a bood drive.
      ## - If there is any kind of volume info, pick first volume
      lines = run_tool(0, nil, ["-adpBootDrive", "-set", "-l#{volume_id}", "-a#{controller.id}"])
      lines.each do |line|
        @logger << line
      end if @logger
    end

    def set_boot(controller, volume)
      _set_boot(controller,volume.id)
    end

    def clear_controller_config(controller, type)
      @logger << "clearing controller config for #{controller.name}\n" if @logger
      lines = if type == :foreign
        run_tool(0, nil, ["-CfgForeign", "-Clear", "-a#{controller.controller_id}"])
      else
        run_tool(0, nil, ["-CfgClr", "-a#{controller.controller_id}"])
      end
      lines.each do |line|
        @logger << line
      end if @logger
      self.refresh_controller(controller)
    end
  end
end
