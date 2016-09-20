# Copyright (c) 2013 Dell Inc.
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

# Some common classes, constants, and methods for messing with RAID.
module BarclampRaid

  KILO = 1024
  MEGA = KILO * 1024
  GIGA = MEGA * 1024
  TERA = GIGA * 1024

  # The list of raid levels the raid handing methods are expected to support.
  # Individual controllers may or may not support all of these.
  LEVELS = {
    "jbod" => {
      "min_disks" => 1,
      "max_disks" =>  1,
      "type" => "simple",
      "overhead" => 0
    },
    "raid0" => {
      "min_disks" => 1,
      "max_disks" => 1000,
      "type" => "simple",
      "overhead" => 0
    },
    "raid1" => {
      "min_disks" => 2,
      "max_disks" => 2,
      "type" => "simple",
      "overhead" => 1
    },
    "raid5" => {
      "min_disks" => 3,
      "max_disks" => 1000,
      "type" => "simple",
      "overhead" => 1
    },
    "raid6" => {
      "min_disks" => 4,
      "max_disks" => 1000,
      "type" => "simple",
      "overhead" => 2
    },
    "raid00" => {
      "type" => "spanned",
      "span_type" => "raid0"
    },
    "raid10" => {
      "type" => "spanned",
      "span_type" => "raid1"
    },
    "raid50" => {
      "type" => "spanned",
      "span_type" => "raid5"
    },
    "raid60" => {
      "type" => "spanned",
      "span_type" => "raid6"
    }
  }

  # Calculate the min disks, max disks, and disks used for overhead for
  # a given raid type and raid spans.
  # @param type [String] The type of RAID to calculate for
  # @param spans [Fixnum] The number of spans this raid array contains
  # @return [Array(Integer,Integer,Integer)] An array of the minimum number of disks in the array,
  #   the maximum number of disks in the array, and number of disks used as overhead.
  def self.calc_raid_overhead(type,spans=1)
    raise "Cannot calculate overhead for RAID type #{type}" unless LEVELS.key?(type)
    case LEVELS[type]["type"]
    when "simple"
      raise "Cannot calculate overhead for simple RAID type #{type} with #{spans} spans" if spans > 1
      [LEVELS[type]["min_disks"],
       LEVELS[type]["max_disks"],
       LEVELS[type]["overhead"]]
    when "spanned"
      raise "Spanned RAID volumes cannot contain a single span" unless spans > 1
      type = LEVELS[type]["span_type"]
      [LEVELS[type]["min_disks"] * spans,
       LEVELS[type]["max_disks"] * spans,
       LEVELS[type]["overhead"] * spans]
    else
      raise "Unknown key type for #{LEVELS[type]} type #{LEVELS[type]["type"]}"
    end
  end

  # A handy utility method for converting sizes in string form to
  # sizes in bytes.
  # @param s [Integer,String] Either the number of bytes, or a string in the format of "100 GB"
  # @return [Integer] The number of bytes.
  def self.size_to_bytes(s)
    case s
    when /^([0-9.]+)$/ then $1.to_f
    when /^([0-9]+)\s*[Kk][Bb]$/ then $1.to_f * KILO
    when /^([0-9.]+)\s*[Mm][Bb]$/ then $1.to_f * MEGA
    when /^([0-9.]+)\s*[Gg][Bb]$/ then $1.to_f * GIGA
    when /^([0-9.]+)\s*[Tt][Bb]$/ then $1.to_f * TERA
    else
      s.to_f
    end.ceil
  end

  # Calculate how many spans are needed for the RAID type and number of disks.
  # @param raid_level [String] The raid level to calculate for.
  # @return [Array(Integer,Integer)] An array containing the number of disks that will be used in the array,
  #  and the total number of spans.
  def self.calc_spans(raid_level,disks)
    case raid_level
    when "jbod","raid0","raid1","raid5","raid6" then [disks,1]
    when "raid00","raid50","raid60" then [(disks >> 1) << 1,2]
    when "raid10" then [(disks >> 1) << 1, disks >> 1]
    else
      raise "Cannot calculate disk spans for RAID type #{type}"
    end
  end

  # Given a target volume size, a raid type, the number of disks in the raid, and the
  # stripe size, figure out how much space on each physical disk the target volume needs
  # to occupy.  This function deliberately overestimates the amount of space taken.
  #
  # @param volume_size [Integer] The size of the volume in bytes.
  # @param raid_level [String] The type of RAID volume this is,
  # @param disks [Integer] The total number of disks in the RAID.
  # @param spans [Integer] The number of spans in the RAID array.  Defaults to 1.
  # @param stripe_size [Integer]The stripe size if individual stripes in this array.
  #   Defaults to 64KB
  #
  # @return [Integer]: The number of bytes the RAID volume will occupy on each physical disk.
  def self.raid_per_disk_size(volume_size,raid_level,disks,spans=1,stripe_size=65536)
    # Overestimate the number of stripes per disk
    _, _, overhead = BarclampRaid.calc_raid_overhead(raid_level,spans)
    stripes_per_disk = (volume_size / (stripe_size * (disks - overhead))).ceil
    stripes_per_disk * stripe_size * disks
  end

  # A hash that allows read access to its keys as methods.
  class HashBucket < ActiveSupport::HashWithIndifferentAccess

    def method_missing(meth, *args, &block)
      return self[meth] if args.length == 0 && self.key?(meth)
      super
    end

  end

  # BarclampRaid::Controller holds everything we need to manipulate
  # a RAID controller.  It is mostly just a collection of peoperties exposed by the
  # underlying {BarclampRaid::Driver} that owns it.
  class Controller < HashBucket

    # Returns the driver that manages this controller.
    attr_reader :driver

    # When a Controller is created, these keys must be present in the hash
    # passed to Controller.new.
    @@required_keys = [
                       "id",                   # string to identify the controller.
                       "driver",               # a reference to the driver class
                       "driver_name",          # the name of the driver used to manage this controller.
                       "bus",                  # bus number (integer)
                       "device",               # device bus number (integer)
                       "function",             # function bus number (integer)
                       "firmware_version",     # version of the firmware on the drive.
                       "device_id",            # device id as reported by card
                       "vendor_id",            # vendor id as reported by card
                       "sub_device_id",        # sub device id as reported by card
                       "sub_vendor_id",        # sub vendor id as reported by card
                       "product_name",         # name of product (LSI2008 will be lame)
                       "raid_capable",         # boolean if raid is allowed or not
                       "supported_raid_levels" # list of symbols of supported RAID levels
                      ]

    # Simple unique name.  Ideally, this will be unique across all raid controllers.
    def name
      "#{@driver.name}:#{self["id"]}"
    end

    # Utility method to make backtraces and debugging easier by
    # killing irrelavent detail.
    def inspect
      "<#{self.class.name}: #{self.name}>"
    end

    # Create a new Controller.  As a side effect, it populates volume and physical disk information.
    # @param h [Hash] A hash that contains at least the keys in @@required_keys
    def initialize(h = {})
      super(h)
      missing_keys = @@required_keys - self.keys
      unless missing_keys.empty?
        raise "BarclampRaid::Controller initialized with a hash missing required keys #{missing_keys.inspect}"
      end
      @driver = self.delete("driver")
      @driver.refresh_controller(self)
    end

    # Get the disks that are members of a volume.  Note that this does not tell you which
    # volumes the disks are members of.
    def used_disks
      self.volumes.reduce([]) do |memo,obj|
        (memo + obj.disks).uniq
      end
    end

    # Get disks that are not members of a volume.
    def unused_disks
      self.disks - self.used_disks
    end

    # Given a volume specification hash, find disks on the controller that are
    # good candidates for including in the to-be-created volume.
    # This method takes into account the desired size of the volume,
    # whether or not this volume can share disks with other volumes,
    # the type of disks to use in the volume, whether SAS or SATA volumes
    # should be chosen, and it makes sure that all chosen disks are
    # of the same time and approximately the same size.
    # @param volume [Hash] A hash that contains the salient details of the volume.
    #   See {Driver#create_vd} for details on what keys are allowed and what they mean.
    # @return [Array<BarclampRaid::RaidDisk>] The disks that should be used to create
    #  the volume.
    def find_candidates(volume)
      raid_level  = volume["raid_level"]
      disks       = volume["disks"]
      size        = volume["size"] || "max"
      exclusive   = (volume["exclusive"].nil? || size.nil?) ? true : !!volume["exclusive"]
      disk_type   = volume["disk_type"]
      protocol    = volume["protocol"]
      stripe_size = BarclampRaid.size_to_bytes(volume["stripe_size"] || "64 KB")
      disks, spans = BarclampRaid.calc_spans(raid_level,disks)
      min_disks, max_disks, _ = BarclampRaid.calc_raid_overhead(raid_level,spans)
      raise "RAID type #{raid_level} requires at least #{min_disks} disks" unless disks >= min_disks
      raise "RAID type #{raid_level} cannot have more than #{max_disks} disks" unless disks <= max_disks
      candidate_sizes = Hash.new
      candidates = Array.new
      if exclusive
        # Only look at disks that do not have any RAID volumes on them.
        candidates = self.unused_disks
        candidates.each{|c|candidate_sizes[c.id] = c.disk_size}
      else
        # Look at all the disks, and figure out how much free space each drive has.
        candidates = self.diskscontroller = BarclampRaid::RaidHammer.drivers(Node.find(2))[0].controllers[0]
        candidates.each do |c|
          candidate_sizes[c.id] = c.disk_size
        end
        self.volumes.each do |vol|
          disk_size = vol.size_per_disks
          vol.disks.each do |d|
            candidate_sizes[d.id] -= disk_size
          end
        end
      end
      # Bucketize candidates according to disk_type and protocol. Most RAID controllers
      # rewuire that the drives be fairly homogenous, so we enforce that all of the disks
      # in our prospective RAID volume talk the same protocol and are the same media type.
      type_buckets = Hash.new
      candidates.each do |c|
        type_buckets[[c.protocol,c.media_type]] ||= Array.new
        type_buckets[[c.protocol,c.media_type]] << c
      end
      chosen_buckets = Array.new
      (disk_type ? [disk_type] : ["disk","ssd"]).each do |c_mt|
        (protocol ? [protocol] : ["sas","sata"]).each do |c_p|
          selector = [c_p,c_mt]
          next unless type_buckets[selector]
          if type_buckets[selector].length < disks
            Rails.logger.info("#{self.class.name}: Discarding disk type #{selector.inspect} due to insufficient disks")
            next
          end
          chosen_buckets << type_buckets[selector]
          break
        end
      end
      raise "Not enough combinable disks to satisfy volume creation request!" if chosen_buckets.empty?
      # Bucketize the disks in each chosen bucket by size.  Base the sizes
      # on candidate_sizes, rounded down to the nearest 32 gigabytes.
      # This is hardly a perfect heuristic, but it should work well enough.
      res = []
      chosen_buckets.each do |bucket|
        break unless res.empty?
        size_buckets = Hash.new
        bucket.each do |disk|
          size_bucket = candidate_sizes[disk.id] >> 35 << 35
          size_buckets[size_bucket] ||= Array.new
          size_buckets[size_bucket] << disk
        end
        size_candidates = case size
                          when "max" then size_buckets.keys.sort.reverse
                          when "min" then size_buckets.keys.sort
                          else
                            size_per_disk = BarclampRaid.raid_per_disk_size(
                                                                            BarclampRaid.size_to_bytes(size),
                                                                            raid_level,
                                                                            disks,
                                                                            spans,
                                                                            stripe_size)
                            size_buckets.keys.sort.reject{|b| b < (size_per_disk >> 35 << 35)}
                          end
Rails.logger.error("GREG: size_candidates = #{size_candidates.inspect}")
        size_candidates.each do |c|
          next unless size_buckets[c].length >= disks
          res = size_buckets[c]
          break
        end
      end
      if res.empty?
        raise "Not enough compatible disks on controller #{self.name} to build #{volume.inspect}"
      end
      # Now we have the candidates sorted in the order we should grab disks from.
      # Grab the disks we should use, sorted in ascending order by disk ID.
      res.sort.take(disks)
    end

    # Create a volume.  This is a helper that hands the request off to the appropriate driver.
    # @param volume [Hash] A hash of the same type that {Driver#create_vd} takes.
    # @return [BarclampRaid::Volume] The created volume.
    def create_vd(volume)
      @driver.create_vd(self,volume)
    end

    # Clear everything from the controller.
    # This also just hands off the request to the appropriate driver.
    # @param config_type [:foreign, :all] What kind of configuration to clear from the system.
    def clear_config(config_type = :all)
      @driver.clear_controller_config(self,config_type)
    end

    # Delete a virtual disk.
    # This also just hands the request off to the appropriate controller.
    # @param volume [BarclampRaid::Volume] The volume to delete.
    def delete_vd(volume)
      @driver.delete_vd(volume)
    end

  end

  # Store information about a RAID volume.
  class Volume < HashBucket

    # The controller that owns this volume
    attr_reader :controller

    # The keys that must be present in the hash passed to new
    @@required_keys = [
                       "id",            # driver returned volume id (string)
                       "name",          # driver returned volume name (string)
                       "status",        # Driver returned status (string)
                       "raid_level",    # Driver returned raid level (symbol)
                       "vol_size",      # Driver returned size (int) (bytes)
                       "controller",    # Reference to the controller that owns this volume
                       "stripe_size",   # The size in bytes of each stripe
                       "spans",         # Number of spans in this volume.
                       "span_length",   # integer Number of disks per span
                       "disks"         # Array of RaidDisks that make up this volume.
                      ]
    # @param h [Hash] A hash that contains the keys in @@required_keys
    def initialize(h = {})
      super(h)
      missing_keys = @@required_keys - self.keys
      unless missing_keys.empty?
        raise "BarclampRaid::Volume initialized with a hash missing required keys #{missing_keys.inspect}"
      end
      @controller = self.delete("controller")
      self["driver_name"] = @controller.driver_name
      self["controller_id"] = @controller.id
    end

    # Calculate approximately how much space per disk this volume takes up.
    # This just hands off to the module-provided raid_per_disk_size method.
    # @return [Integer] The number of bytes this volume uses on each physical disk
    #  it uses as backing store.
    def size_per_disk
      BarclampRaid.raid_per_disk_size(self.vol_size,
                                      self.raid_level,
                                      self.disks.length,
                                      self.spans,
                                      self.stripe_size)
    end

    # Delete this virtual disk.
    def delete_vd
      @controller.delete_vd(self)
    end
  end

  # Store information about a particular physical disk attached to a RAID controller.
  class RaidDisk < HashBucket

    # The controller that owns this physical disk
    attr_reader :controller

    # The keys that must be present in the hash passed to new.
    @@required_keys = [
                       "controller",    # Reference to controller
                       "disk_size",     # Driver returned size in bytes as integer
                       "enclosure",     # Driver returned enclosure id
                       "slot",          # Driver returned slot
                       "sas_address",   # String
                       "protocol",      # string, "sas" or "sata"
                       "media_type",    # string, "disk" or "ssd"
                       "status"         # String
                      ]

    # @param h [Hash] A hash that contains the keys in @@required_keys
    def initialize(h = {})
      super(h)
      missing_keys = @@required_keys - self.keys
      unless missing_keys.empty?
        raise "BarclampRaid::RaidDisk initialized with a hash missing required keys #{missing_keys.inspect}"
      end
      @controller = self.delete("controller")
      self["driver_name"] = @controller.driver_name
      self["controller_id"] = @controller.id
    end

    # Generate a unique ID based on the enclosure and slot the disk is attached to.
    def id
      "#{self.enclosure}:#{self.slot}"
    end

    # Allow disks to be sorted based on enclosure ID.
    def <=>(other)
      self.id <=> other.id
    end

  end

  # All specific RAID drivers should inherit from this driver class.
  class Driver

    # Creates a new driver conifgured to control RAID arrays on node
    # @param node [Node] A Rebar node object.
    # @param logger [StringIO] A StringIO buffer to accumulate user feedback
    # @param params [Hash] The corresponding entry from Attrib.get(node,'raid-drivers')
    def initialize(node,logger,params)
      @node = node
      @logger = logger
      @params = params
    end

    # The name of the driver.
    def name
      @params["name"]
    end

    # Keep debugging and backtraces for being too verbose.
    def inspect
      self.class.name.to_s
    end

    # Test to see if this driver is useable.
    # @return [Boolean]
    def useable?
      raise Exception.new("MUST IMPLEMENT THIS")
    end

    # @return [Array<BarclampRaid::Controller>] The controllers that this driver should be used to manage.
    def controllers
      raise Exception.new("MUST IMPLEMENT THIS")
    end

    # @param controller [BarclampRaid::Controller] The controller to enumerate disks from
    # @return [Array<BarclampRaid::RaidDisk>] The physical disks that this controller manages.
    def disks(controller)
      raise Exception.new("MUST IMPLEMENT THIS")
    end

    # @param controller [BarclampRaid::Controller] The controller to enumerate volumes from
    # @return [Array<BarclampRaid::Volume>] The volumes that this raid array manages
    def volumes(controller)
      raise Exception.new("MUST IMPLEMENT THIS")
    end

    # Refresh the physical disk and volume information for a controller.
    # @param controller [BarclampRaid::Controller] The controller pull current disk and volume information from.
    def refresh_controller(controller)
      @logger << "#{controller.name} refreshed\n" if @logger
      controller["disks"] = disks(controller)
      controller["volumes"] = volumes(controller)
    end

    # Clear configuration from a controller
    # @param controller [BarclampRaid::Controller] The controller to clear config from.
    # @param config_type [:foreign, :all] The configuration to clear on the controller.
    #  :all = clear all configuration information from the controller,
    #  :foreign = clear just foriegn configuration from the controller.
    def clear_controller_config(controller, config_type = :all)
      raise Exception.new("MUST IMPLEMENT THIS")
    end

    # Create a virtual disk.
    # @param controller [BarclampRaid::Controller] controller object from enumerate
    # @param volume [Hash] hash containing the following keys:
    #
    # The volume hash must contain keys from the following table:
    # "raid_level":: string containing the desired raid level.
    # "name":: The name of the volume.  If omitted, one that is
    #           unique-ish will be created.
    # "size":: The desired size of the volume.  This can be:
    #              "min":: Make a RAID volume out of the smallest disks
    #                      available (if creating an exclusive volume),
    #                      or create a RAID volume out of the smallest
    #                      amount of free space on shareable physical
    #                      disks (if not operating in exclusive mode).
    #              "max":: Make a RAID volume out of the largest disks
    #                      available (if operating in exclusive mode),
    #                      or out of the largest amount of free space
    #                      available (including entire free disks) if
    #                      not operating in exclusive mode.
    #                      Defaults to "max"
    #              Integer:: Create a RAID volume with approximately
    #                        the size of the passed integer in bytes.  The RAID
    #                        volume may be larger than the passed size based on
    #                        the passed stripe size and the number of spindles in
    #                        raid volume.
    # "stripe_size":: The stripe size of the volume.  It must be a power
    #                 of two, and it defaults to 64KB.
    # "disks":: The disks to use to build the array.
    #           If this is an array, then all the entries in the array
    #           must be BarclampRaid::RaidDisks, and the next 3 possible
    #           entries in the hash will be ignored.
    #           If this is a number, then we will try to pick appropriate
    #           disks based on the rest of the parameters in the hash by
    #           passing the volume hash to controller.find_candidates.
    # "exclusive":: Whether the volume shold be allowed to share physical
    #               disks with other volumes.  Defaults to true, meaning
    #               the volume cannot share physical disks.
    # "disk_type":: What kind of disks should be used to build the volume.
    #               Can be unset, "ssd", or "disk".  If unset, we will
    #               prefer "disk" over "ssd".
    # "protocol":: The protocol used to talk to the disks. It can be
    #              unset, "sata", or "sas".  If unset, will prefer "sas".
    def create_vd(controller, volume)
      raise Exception.new("MUST IMPLEMENT THIS")
    end

    # Delete a volume.
    # @param volume [BarclampRaid::Volume] Volume to delete.
    def delete_vd(volume)
      raise Exception.new("MUST IMPLEMENT THIS")
    end

    # Mark a volume as bootable.
    # @param volume [BarclampRaid::Volume] Volume to mark as bootable.
    #   This is a one-per-controller setting, so it is possible for more
    #   than one volume to be marked as bootable in the system.
    def set_boot(volume)
      raise Exception.new("MUST IMPLEMENT THIS")
    end

    #
    # FUTURE is almost now!
    #
    # Controller = controller object from enumerate
    # Volume = Volume object from enumerate [Optional]
    #
    # def create_spare(controller, volume)
    #   raise Exception.new("MUST IMPLEMENT THIS")
    # end

    # split a line into a key/value pair based on the specified regex
    # @param line [String] The line to split into a key/value pair.
    # @param re [Regexp] The regular expression to split lines on.
    #   The regular expression must capture the key and the value
    # @return [Array(String,String)] The key and value matched by re
    def extract_value(line, re = /([^:]*):(.+)$/)
      re.match(line.strip)
      [($1.strip rescue ""), ($2.strip rescue "")]
    end

    # Given an array of lines, split it into several arrays
    # with re.  Lines before the first match of re will be ignored.
    # @param lines [Array<String>] The lines to split into groups
    # @param re [Regexp] A regular expression that matches lines that should be used to split on.
    # @return [Array<Array<String>>] The sections that lines was split into.
    def split_lines_on(lines,re)
      sections = []
      section = nil
      lines.each do |line|
        if line =~ re
          sections << section if section
          section = Array.new
        end
        section << line if section
      end
      sections << section if section
      sections
    end

    #####
    #  Execute the given command, and check for errors.
    #
    def run_command(cmd, success, error)
      log "will execute #{cmd}"
      out,_,status = @node.run(cmd)
      if ((error and status.exitstatus == error) or (success and status.exitstatus != success))
        raise "cmd #{cmd} returned #{status.exitstatus}. #{out}"
      end
      out.lines
    end

    def run_tool(success, error, args)
      cmd = [@params["executable"], *args]
      cmdline = cmd.join(" ")
      run_command(cmdline, success, error)
    end

    def log( msg, sev=:DEBUG)
      case sev
      when :DEBUG then Rails.logger.debug(msg)
      when :ERROR then Rails.logger.error(msg)
      end
    end
  end
end
