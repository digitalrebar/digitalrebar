#!/usr/bin/ruby
# Copyright (c) 2014 Dell Inc.
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
# This file contains the driver that all BIOS configuration/update
# classes should inherit from.

module BarclampBios

  # Encapsulate our knowledge about a specific BIOS setting.
  # This includes its common name, current value, default value, and proposed value.
  # It also includes utility methods for proposing a new setting
  # and refreshing itself
  class Setting
    attr_reader :name, :default_value, :current_value, :driver, :proposed_value, :validator
    attr_reader :opts

    # The keys that must be present in the hash passed to Setting.new
    @@required_keys = ["name",
                       "driver",
                       "current_value"]
    # Create a new BarclampBios::Setting.
    # @param opts [Hash] The initial values for this setting.
    def initialize(opts = {})
      missing_keys = @@required_keys - opts.keys
      unless missing_keys.empty?
        raise "Missing required keys #{missing_keys.inspect}"
      end
      @name = opts.delete("name")
      @driver = opts.delete("driver")
      @current_value = opts.delete("current_value")
      @proposed_value = opts.delete("proposed_value") || @current_value
      @validator = opts.delete("validator")
      @opts = opts.dup.freeze
    end

    # Set a new proposed value.  Will call @validator with the new value to verify
    # that it is a permissible value.
    # @param v The value to propose.
    def proposed_value=(v)
      case
      when validator.is_a?(Array)
        unless validator.member?(v)
          raise("BarclampBios::Setting #{@name} cannot be set to #{v}\nValid values are: #{validator.inspect}")
        end
      when validator.is_a?(Range)
        unless validator.member?(v)
          raise("BarclampBios::Setting #{@name} cannot be set to #{v}\nValid values are between: #{validator.first} and #{validator.last}")
        end

      when validator.is_a?(Hash)
        unless validator["length"].member?(v.length) && validator["regex"] =~ v
          raise("BarclampBios::Setting #{@name} cannot be set to #{v}\nValid values have lengths between: #{validator["length"].first} and #{validator["length"].last}, and match #{validator["regex"].to_s}")
        end
      else
        raise("Cannot handle validator of type #{validator.class.name}")
      end
      @proposed_value = v
    end

    def to_hash
      res = {}
      res["name"] = @name
      res["current_value"] = @current_value
      res["proposed_value"] = @proposed_value
      case
      when @validator.is_a?(Range)
        res["type"] = "integer"
        res["validator"] = {
          "range" => [@validator.first,@validator.last]
        }
      when @validator.is_a?(Array)
        res["type"] = "list"
        res["validator"] = {
          "values" => @validator
        }
      when @validator.is_a?(Hash)
        res["type"] = "string"
        res["validator"] = {
          "length_range" => [@validator["length"].first,@validator["length"].last],
          "regex" => @validator["regex"].to_s
        }
      else
        raise "Cannot handle validator type #{@validator.class.name}: #{@validator.inspect}"
      end
      res
    end
  end

  # The base class that all Bios drivers should inherit from and implement.
  class Driver

    # Capture a reference to the node we are going to manage BIOS configuration
    # and firmware updates on.  It assumes that all needed tools are installed,
    # and that any required OOB communication channels have been configured.
    # @param node [Node] The node that this instance of the driver will manage.
    def initialize(node)
      @node = node
    end

    # Provide a user-friendly inspect method.
    def inspect
      "<#{self.class.name}: #{@node.name}>"
    end

    # Return an array of settings that this driver can manage.
    # Subclasses must implement this so that the first call to
    # settings caches the BIOS settings in an instance variable named
    # settings.
    # @return [Hash{String => BarclampBios::Setting}]
    def settings
      raise "Must be implemented by subclass!"
      @settings
    end

    # Refresh this driver's settings by setting @settings to nil,
    # and then calling the settings method
    # @return [Hash{String => <BarclampBios::Setting>}]
    def refresh
      @settings = nil
      self.settings
    end

    # Shorthand for getting the proposed (or current, if no value has been proposed)
    # value of a setting. If you want the whole setting object, use
    # {BarclampBios::Driver#settings[]}
    # @param key [String] The name of the setting
    # @return The proposed (or current if no value has been proposed) value.
    def [](key)
      self.settings[key].proposed_value
    end

    # Shorthand for proposing a new value for a BIOS setting.
    # @param k [String] The name of the setting to propose a new value for.
    # @param v The value to propose
    # @return The new proposed value
    # @raise [RuntimeError] if the proposed value is not valid for this setting.
    def []=(k,v)
      self.settings[k].proposed_value = v
    end

    # Commit the new settings. This may cause the node to require a reboot..
    # @param s [Array<BarclampBios::Setting>] The settings that should be committed.
    #   All settings in the array must have been returned by a previous call to settings for this driver.
    #   An exception will be raised if this is not the case.
    #   Additionaly, an exception will be raised if we are unable to commit changes for some reason.
    # @return [Boolean] Whether or not the node needs to be rebooted.
    def commit
      raise "Must be implemented by subclass!"
      false
    end

    # Wipe the BIOS settings back to the factory defaults.
    # Please note that this does not ensure that the node will PXE boot
    # by default.
    def factory_reset!
      raise "Must be implemented by subclass!"
      false
    end
  end
end
