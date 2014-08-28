#!/usr/bin/ruby
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

#
# ./wsman_attr_diff.rb <IP> root crowbar \
# /opt/dell/barclamps/bios/chef/data_bags/crowbar-data/bios-set-PowerEdgeR720xd-default.json \
# /opt/dell/barclamps/bios/chef/data_bags/crowbar-data/bios-set-PowerEdgeR720xd-Hadoop.json
#

require 'rubygems'
require 'chef'
require 'wsman'
require 'wsman_attributes'

def merge_hash(base, adds)
  adds["attributes"].each do |k, groups|
    if base["attributes"][k]
      groups.each do |group, attrs|
        if base["attributes"][k][group]
          base["attributes"][k][group] = base["attributes"][k][group].merge(attrs)
        else
          base["attributes"][k][group] = attrs
        end
      end
    else
      base["attributes"][k] = groups
    end
  end
  base
end

# Helper function to handle wildcarding in the attributes file
def test_value(item, attributes)
  return true, "Item #{item.instance_id} is not set from file (read-only)" if item.is_read_only

  # Build fqdd and group
  fqdd = item.fqdd ? item.fqdd : "LC"
  group_id = item.group_id
  group_id = nil if group_id and not item.instance_id.include?("#{group_id}#")
  group_id = group_id ? group_id : "default"
  attr_name = item.attribute_name

  # Find best matching fqdd
  fqdd_part = attributes[fqdd]
  if fqdd_part.nil?
    head, sep, attr = fqdd.rpartition(/[#.:]/)
    while head != ""
      fqdd_part = attributes[head]
      break if fqdd_part
      head, sep, attr = head.rpartition(/[#.:]/)
    end
  end
  unless fqdd_part
    return true, "Item #{item.instance_id} is not set from file (fqdd)"
  end

  # Find best matching group
  group_part = fqdd_part[group_id]
  if group_part.nil?
    head, sep, attr = group_id.rpartition(/[#.:]/)
    while head != ""
      group_part = fqdd_part[head]
      break if group_part
      head, sep, attr = head.rpartition(/[#.:]/)
    end
  end
  
  unless group_part
    return true, "Item #{item.instance_id} is not set from file (group)"
  end

  # Return value if it exists.
  unless group_part[attr_name]
    return true, "Item #{item.instance_id} is not set from file (attribute)"
  end

  badness = group_part[attr_name]["value"] == item.current_value
  str = "Item #{item.instance_id} should be set to #{group_part[attr_name]["value"]} and is #{item.current_value}"
  return badness, str
end

def diff_attributes(opts, files)
  wsman = Crowbar::BIOS::WSMAN.new(opts)
  wsman_attributes = Crowbar::BIOS::WSMANAttributes.new(wsman)

  # Load files
  test_attrs = {}
  test_attrs["attributes"] = {}
  files.each do |file|
    tmpjson = File.read(file)
    new_data = JSON.parse(tmpjson)
    test_attrs = merge_hash(test_attrs, new_data)
  end

  # Load attributes
  attrs = []
  h = wsman_attributes.attributes("DCIM_BIOSEnumeration")
  attrs << h
  h = wsman_attributes.attributes("DCIM_BIOSString")
  attrs << h
  h = wsman_attributes.attributes("DCIM_BIOSinteger")
  attrs << h
  h = wsman_attributes.attributes("DCIM_NICAttribute")
  attrs << h
  h = wsman_attributes.attributes("DCIM_iDRACCardAttribute")
  attrs << h
  h = wsman_attributes.attributes("DCIM_RAIDAttribute")
  attrs << h
  h = wsman_attributes.attributes("DCIM_LCAttribute")
  attrs << h

  puts

  attrs = attrs.flatten!

  results = ""
  json = Hash.new
  json["attributes"] = {}
  attrs.each do | item |
    fqdd = item.fqdd ? item.fqdd : "LC"
    fqdd = fqdd.split(/[.:]/)[0]
    json["attributes"][fqdd] = {} unless json["attributes"][fqdd]
    group_id = item.group_id
    group_id = nil if group_id and not item.instance_id.include?("#{group_id}#")
    group_id = group_id ? group_id : "default"
    group_id = group_id.split(/[.:]/)[0]
    json["attributes"][fqdd][group_id] = {} unless json["attributes"][fqdd][group_id]

    int_range = ""
    str_range = ""
    possible_values = ""
    str_range = "#{item.min_length} #{item.max_length}" if item.is_string
    int_range = "#{item.lower_bound} #{item.upper_bound}" if item.is_integer
    possible_values = "#{item.possible_values.join(",")}" if item.is_enumeration

    json["attributes"][fqdd][group_id][item.attribute_name] = { 
      "instance_id" => item.instance_id, 
      "value" => item.current_value, 
      "attr_name" => item.attribute_name, 
      "GroupID" => item.group_id, 
      "DefaultValue" => item.default_value,
      "ReadOnly" => item.is_read_only,
      "type" => item.type,
      "possible_values" => possible_values,
      "integer_range" => int_range,
      "string_range" => str_range
    }
 
    val, str = test_value(item, test_attrs["attributes"])
    puts "Mismatch: #{str}" unless val
    results = results + "\n" + str
  end

  puts "Detailed results"
  puts results

  puts "Attributes in the config files"
  puts JSON.pretty_generate(test_attrs)

  puts "Attributes on the current system"
  puts JSON.pretty_generate(json)
end


opts = { :host => ARGV.shift, :user => ARGV.shift, :password => ARGV.shift, :port => 443, :debug_time => false }
files = ARGV

diff_attributes(opts, files)
