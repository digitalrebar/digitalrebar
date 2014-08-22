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

####
# Some common data structures used by all RAID support libraries

$in_chef = true

class Crowbar
  class RAID
    
    RAID0 = :RAID0
    RAID1 = :RAID1      
    RAID5 = :RAID5
    RAID6 = :RAID5
    RAID1E = :RAID1E
    RAID10 = :RAID10
    RAID50 = :RAID50
    RAID60 = :RAID60
    JBOD = :JBOD
    
    KILO = 1024
    MEGA = KILO * 1024
    GIGA = MEGA * 1024
    TERA = GIGA * 1024  
    
    @@controller_styles = []

    ### 
    # Catch classes that inherit us, so we have a list of controller styles to try to use
    def self.inherited(subclass)      
      @@controller_styles  << subclass
    end
    
    def self.controller_styles
      @@controller_styles
    end

    class Driver
      attr_accessor :debug

      #
      # Returns a string describing this driver
      #
      def describe
        raise Exception.new("MUST IMPLEMENT THIS")
      end

      #
      # Returns a list of controllers (with current disks and volumes attached)
      #
      def enumerate_topology
        raise Exception.new("MUST IMPLEMENT THIS")
      end

      #
      # Controller = controller object from enumerate
      # config_type = :foreign or :all
      #
      def clear_controller_config(controller, config_type)
        raise Exception.new("MUST IMPLEMENT THIS")
      end

      #
      # Controller = controller object from enumerate
      # volume_description = hash of the following
      #    :type => Raid Level Symbol
      #    :name => Name of volume
      #    :size => Bytes or string MAX
      #    :disks => List of RaidDisk 
      #
      def create_vd(controller, volume_description)
        raise Exception.new("MUST IMPLEMENT THIS")
      end

      #
      # Volume = Volume object from enumerate
      #
      def delete_vd(volume)
        raise Exception.new("MUST IMPLEMENT THIS")
      end

      #
      # Controller = controller object from enumerate
      # Volume = Volume object from enumerate
      # NIC First = optional parameter indicating if NIC should be first or not.
      #
      def set_boot(controller, volume, nic_first = true)
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

      ### HELPER FUNCTIONS
      #
      # Extract the value from text following the pattern:
      #
      #  Label with some words: <value>    
      def extract_value(line, re = /^(.*)\s*:\s(.*)$/)
        re.match(line.strip)
        key = $1.strip rescue ""
        value = $2.strip rescue ""
        [key, value]
      end
    
      ###
      # Skip items in the lines string array until a line matching the given Regexp is found.
      # return the skipped lines. The matching line is the lines[0]
      #
      def skip_to_find(lines, re) 
        skipped = []
        skipped << lines.shift while lines.length > 0 and re.match(lines[0]).nil?
        log("first line is:#{lines[0].nil? ? '-' : lines[0]}")
        skipped
      end
    
      ##### 
      #  Execute the given command, and check for errors.
      #  
      def run_command(cmd, success, error, &block)    
        log "will execute #{cmd}"
        if block_given?
          ret = IO.popen(cmd,&block)            
          log("return code is #{$?}")
          raise "cmd #{cmd} returned #{$?}" if ((error and $? == error) or (success and $? != success))
          return ret
        else
          text = ""
          IO.popen(cmd) {|f|
            text = f.readlines
          }
          raise "cmd #{cmd} returned #{$?}. #{text}" if ((error and $? == error) or (success and $? != success))
        end
        text
      end
 
      def log( msg, sev=:DEBUG)
        if $in_chef == true 
          case sev 
            when :DEBUG
            Chef::Log.info(msg) if @debug
            when :ERROR
            Chef::Log.error(msg)
          end          
        else
          puts msg if @debug
        end        
        true        
      end      
    
      def size_to_bytes(s)
        case s
          when /^([0-9.]+)$/
          return $1.to_f

          when /^([0-9]+)\s*[Kk][Bb]$/
          return $1.to_f * KILO

          when /^([0-9.]+)\s*[Mm][Bb]$/
          return $1.to_f * MEGA

          when /^([0-9.]+)\s*[Gg][Bb]$/
          return $1.to_f * GIGA

          when /^([0-9.]+)\s*[Tt][Bb]$/
          return $1.to_f * TERA
        end
        -1
      end
    end

    #
    # A bucket of info stored in a hash, but accessible by x.data_part
    #
    class HashBucket
      attr_accessor :attr_hash
    
      def initialize(hash = {})
        @attr_hash = hash.dup if hash
      end

      def set(key, value)
        @attr_hash[key.to_sym] = value
      end

      def method_missing(meth, *args, &block)
        if args.length == 0
          @attr_hash[meth]
        elsif args.length == 1 and meth.to_s =~ /=$/
          #sublime syntax highlighting seems to go nuts with /=/
          #so use the equivalent %r|xxx|
          meth_sym = meth.to_s.gsub(%r|=| ,'').to_sym 
          @attr_hash[meth_sym] = args[0]
        else
          super
        end
      end

      def to_hash
        @attr_hash.dup
      end
    end

=begin
    # Expected attributes:
    #   controller_id = string to identify the controller (may need driver id)
    #   driver = reference to containing driver.
    #   bus = bus number (integer)
    #   device = device bus number (integer)
    #   function = function bus number (integer)
    #   firmware_version = String version
    #   device_id = device id as reported by card
    #   vendor_id = vendor id as reported by card
    #   sub_device_id = sub device id as reported by card
    #   sub_vendor_id = sub vendor id as reported by card
    #   product_name = name of product (LSI2008 will be lame)
    #   raid_capable = boolean if raid is allowed or not
    #   supported_raid_levels = list of symbols of supported RAID levels
    #
    #   bios_version = String version (optional)
    #   pci_address = string of pci address (optional)
    #   limitations in variables are optional
=end
    class Controller < HashBucket
      attr_accessor :driver, :disks, :volumes, :avail_disks

      def initialize(hash = {})
        super(hash)
        @disks = []
        @volumes = []
      end

      def to_hash
        ans = super
        ans["disks"] = []
        disks.each { |d| ans["disks"] << d.to_hash }
        ans["volumes"] = []
        volumes.each { |v| ans["volumes"] << v.to_hash }
        ans
      end
    end
    
=begin
    # Expected attributes:
    #   vol_id - driver returned volume id (string)
    #   vol_name - driver returned volume name (string)
    #   status - Driver returned status (string)
    #   raid_level - Driver returned raid level (symbol)
    #   size - Driver returned size (int) (bytes)
    #   controller - Reference to controller
    #
    #   strip_size - integer in KB (optional)
    #   span_depth - integer (optional)  (Number of spans)
    #   span_length - integer (optional) (Number of drives per span)
    #
=end
    class Volume < HashBucket
      attr_accessor :controller, :members
      
      def initialize(hash = {})
        super(hash)
        @members = []
      end
      
      def to_s
        disks = members.map {|d| d.to_s }.sort.join(",")
        "id: #{self.vol_id} name: #{self.vol_name} type: #{self.raid_level} members:#{disks}"
      end
      
      def to_hash
        h = super
        h[:members] = []
        members.each { |m| h[:members] << m.to_hash }
        h
      end
    end
    
=begin
    # Expected attributes:
    #   controller - Reference to controller
    #   size - Driver returned size in bytes as integer
    #   enclosure - Driver returned enclosure id
    #   slot - Driver returned slot
    #   model - String
    #   manufacturer - String
    #   serial_number - String
    #   sas_address - String
    #   protocol - symbol - :SAS :SATA
    #   media_type - symbol - :SSD, :DISK
    #   status - String
    #
=end    
    class RaidDisk < HashBucket
      attr_accessor :controller
      
      def to_s
        "#{enclosure}:#{slot}-#{vol_id}"
      end
    end
    
    class OSDisk < HashBucket
    end
    
  end
end
