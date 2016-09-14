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
# This file contains the logic needed to do BIOS configuration and
# updates for Dell Poweredge R-series gear.
# It assumes that we can talk to the IPMI controller embedded on the system,
# and that the system has WSMAN enabled.

# Implement everything needed to manage the attributes we care about on
# a Dell R series box.
class BarclampBios::Dellrseries < BarclampBios::Driver

  # The WSMAN namespaces we are going to get and set our BIOS configs from.
  @@namespaces=["http://schemas.dell.com/wbem/wscim/1/cim-schema/2/DCIM_BIOSEnumeration",
                "http://schemas.dell.com/wbem/wscim/1/cim-schema/2/DCIM_BIOSString",
                "http://schemas.dell.com/wbem/wscim/1/cim-schema/2/DCIM_BIOSInteger"]

  def initialize(node, nr)
    super(node, nr)
    @client = @node.hammers.find_by!(name: "wsman")
  end

  # Get the settings from WSMAN and cache them locally.
  def settings
    return @settings if @settings
    res = Hash.new
    @@namespaces.each do |ns|
      update_log("Gathering info from #{ns}")
      items = @client.enumerate(ns).Items
      next unless items
      last_part = ns.split('/').last
      items.each do |item|
        # Skip items we cannot change.
        next if item.IsReadOnly.text == "true"
        i = Hash.new
        i["name"] = item.AttributeName.text
        i["driver"] = self
        # Construct a validator for this setting.
        case last_part
        when "DCIM_BIOSEnumeration"
          i["current_value"] = item.CurrentValue.text
          # Get all the possible values
          possible_values = Array.new
          item.PossibleValues.each do |pv|
            possible_values << pv.text
          end
          i["validator"] = possible_values
        when "DCIM_BIOSString"
          i["current_value"] = item.CurrentValue.text
          min_length = (item.MinLength.text.to_i rescue nil) || 0
          max_length = (item.MaxLength.text.to_i rescue nil) || 65535
          validate_re = item.ValueExpression.text
          validate_re = /#{(validate_re.nil? || validate_re.empty?) ? '^.*$' : validate_re}/
          i["validator"] = {
            "length" => (min_length..max_length),
            "regex" => validate_re
          }
        when "DCIM_BIOSInteger"
          i["current_value"] = item.CurrentValue.text.to_i
          lower_bound = item.LowerBound.text.to_i
          upper_bound = item.UpperBound.text.to_i
          i["validator"] = (lower_bound..upper_bound)
        else
          raise("Cannot handle BIOS settings of type #{last_part}")
        end
        raise "Aiee!!! Duplicate #{self.inspect} setting #{i["name"]}" if res[i["name"]]
        update_log("Found item: #{item.AttributeName.text} value: #{item.CurrentValue.text}")
        res[i["name"]] = BarclampBios::Setting.new(i)
      end
    end
    @settings = res
  end

  def commit
    # Gather all the changed settings.
    to_commit = @settings.values.reject{|s|s.current_value == s.proposed_value || s.proposed_value.nil?}
    return false if to_commit.empty?

    # Clear any pending config job.
    # We do not care about the results.
    @client.invoke("http://schemas.dell.com/wbem/wscim/1/cim-schema/2/DCIM_JobService","DeleteJobQueue",{"JobID" => "JID_CLEARALL"})
    @client.invoke("http://schemas.dell.com/wbem/wscim/1/cim-schema/2/DCIM_BIOSService","DeletePendingConfiguration",{"Target" => "BIOS.Setup.1-1"})

    args = {
      "Target" => "BIOS.Setup.1-1",
      "AttributeName" => [],
      "AttributeValue" => []
    }
    to_commit.each do |setting|
      args["AttributeName"] << setting.name
      args["AttributeValue"] << setting.proposed_value
    end
    # Propose the changes via WSMAN
    res = @client.invoke("http://schemas.dell.com/wbem/wscim/1/cim-schema/2/DCIM_BIOSService","SetAttributes",args)
    raise("Could not set BIOS settings!\n#{res.to_xml}") if res.ReturnValue.text != "0"
    # Now, create a targeted config job to apply the proposed changes
    # Arguably, we will need to be more intelligent than this once we start handling
    # non-BIOS settings, but this does the job for now.
    res = @client.invoke("http://schemas.dell.com/wbem/wscim/1/cim-schema/2/DCIM_BIOSService",
                         "CreateTargetedConfigJob",
                         { "Target" => "BIOS.Setup.1-1",
                           "RebootJobType" => "3",
                           "scheduledStartTime" => "TIME_NOW"})
    raise("Unable to commit BIOS settings!\n#{res.to_xml}") if res.ReturnValue.text != "4096"
    true
  end

  # Arguably not correct, but it is what we have right now.
  def factory_reset!
    @client.invoke("http://schemas.dell.com/wbem/wscim/1/cim-schema/2/DCIM_LCService",
                   "LCWipe")
  end

end
