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

require "rubygems"
require "yaml"
require "xml"
require "json"


class BaseItem < Hash
  def initialize(attributes)
    attributes.each { |k,v| self[k] = v }
  end

  def add_attributes(attributes)
    attributes.each { |k,v| self[k] = v }
  end
end

class SoftwareComponent < BaseItem
  def initialize(attributes)
    super(attributes)
    self["cid"] = self["path"].split(/[\/\\]/).last
  end

  def cid
    self["cid"]
  end

  def url
    "http://ftp.dell.com/#{self["path"]}"
  end

  def component_type
    self["ComponentType"]
  end

  def package_type
    self["packageType"]
  end

  def package_id
    self["packageID"]
  end

  def name
    self["Name"]
  end

  def devices
    self["SupportedDevices"]
  end

  def vendor_version
    self["vendorVersion"]
  end

  def md5sum
    self["hashMD5"].downcase
  end

  def wanted
    return true if self.component_type == "APAC"
    return true if self.component_type == "FRMW"
    return true if self.component_type == "BIOS"
    false
  end
end

class SoftwareBundle < BaseItem
  def is_win_bundle
    return self["OperatingSystems"]["WIN"]
  end

  def has_platform(name, sys)
    self["Systems"].each do |key, brand|
      return true if brand.has_system(name, sys)
    end      
    return false
  end

  def date_time
    DateTime.parse(self["dateTime"])
  end

  def contents
    self["Contents"]
  end
end

class Brand < BaseItem
  def name
    self["Name"]
  end
  def has_system(brand, sys)
    return false unless brand == self.name
    self["ids"].each do |k,v|
      return true if v == sys
    end
    return false
  end
end

class Device < BaseItem
end

class BundleCatalogCallbacks
  include XML::SaxParser::Callbacks

  attr_reader :bundles, :components

  def initialize 
    @bundles = []
    @components = {}
    @current = nil
  end

  def on_end_element(element)
    return if element == "Manifest"
    return if element == "InventoryComponent"
    return if element == "Contents"
    return if element == "Package"
    return if element == "TargetOSes"
    return if element == "SupportedOperatingSystems"
    return if element == "OperatingSystem"
    return if element == "TargetSystems"
    return if element == "SupportedSystems"
    return if element == "SupportedDevices"
    return if element == "RevisionHistory"
    return if element == "ImportantInfo"
    return if element == "PCIInfo"
    return if element == "Language"
    return if element == "SupportedLanguages"
    return if element == "Category"
    return if element == "ComponentType"
    return if element == "Criticality"
    return if element == "LUCategory"

    if element == "SoftwareBundle" 
      @bundles << @current
      @current = nil
      return
    end
    if element == "SoftwareComponent"
      @components[@current.cid] = @current
      @current = nil
      return
    end

    if element == "Brand"
      hash = @current["Systems"]
      p = @brand["key"]
      hash[p] = @brand
      @brand = nil
      @current["Systems"] = hash
      return
    end

    if element == "Model"
      ids = @brand["ids"]
      ids[@system_id] = @display
      @system_id = nil
      @brand["ids"] = ids
      return
    end

    if element == "Display"
      @display = @buffer 
      # This is bad, but works if Display in Brand is first.
      @brand["Name"] = @display if @brand and @brand["Name"].nil?
      return 
    end

    if element == "Device"
      @device["Name"] = @display
      hash = @current["SupportedDevices"]
      p = @device["componentID"]
      hash[p] = @device
      @device = nil
      @current["SupportedDevices"] = hash
      return
    end

    if element == "Name" or element == "Description"
      @current[element] = @display
      return
    end

    puts "#Element ended: #{element}"
  end

  def on_cdata_block(msg)
    @buffer = @buffer + msg
    @buffer = @buffer.chomp
  end

  def on_characters(chars)
    @buffer = @buffer + chars
    @buffer = @buffer.chomp
  end

  def on_start_element(element, attributes)
    @buffer=""
    if element == "SoftwareBundle" 
      @current = SoftwareBundle.new(attributes)
      return
    end
    if element == "SoftwareComponent"
      @current = SoftwareComponent.new(attributes)
      return
    end
    if element == "Brand"
      @brand = Brand.new(attributes)
      @brand["ids"] = {}
      return
    end
    if element == "Device"
      @device = Device.new(attributes)
      return
    end
    if element == "PCIInfo"
      @device.add_attributes(attributes)
      return
    end

    # Item has value add it to the parent as its name
    if element == "Category" or element == "ComponentType" or element == "Criticality" or element == "LUCategory"
      @current[element] = attributes["value"]
      return
    end

    if element == "Contents"
      @current["Contents"] = {}
      return
    end

    if element == "Package"
      hash = @current["Contents"]
      p = attributes["path"]
      hash[p] = p
      @current["Contents"] = hash
      return
    end

    if element == "TargetSystems" or element == "SupportedSystems"
      @current["Systems"] = {}
      return
    end

    if element == "TargetOSes" or element == "SupportedOperatingSystems"
      @current["OperatingSystems"] = {}
      return
    end

    if element == "OperatingSystem"
      hash = @current["OperatingSystems"]
      p = attributes["osCode"]
      v = attributes["osVendor"]
      hash[p] = v
      @current["OperatingSystems"] = hash
      return
    end

    if element == "ImportantInfo"
      @current["URL"] = attributes["URL"]
      return
    end

    if element == "Model"
      @system_id = attributes["systemID"]
      return
    end

    if element == "SupportedDevices"
      @current["SupportedDevices"] = {}
      return
    end

    return if element == "Manifest"
    return if element == "InventoryComponent"
    return if element == "Language"
    return if element == "SupportedLanguages"
    return if element == "RevisionHistory"
    return if element == "Name"
    return if element == "Description"
    return if element == "Display"

    puts "#Element started: #{element}"
  end

end


###### Script part here #####

crowbar_yml_file=ARGV[0]
files_dir=ARGV[1]

crowbar_yml = YAML.load_file(crowbar_yml_file)
callback = BundleCatalogCallbacks.new
parser = XML::SaxParser.file("#{files_dir}/Catalog.xml")
parser.callbacks = callback
parser.parse

# Build list of bundles that match the yml list.
bundles = {}
crowbar_yml["supported_platforms"].each do |p|
  parts = p.split(" ")
  callback.bundles.each do |b|
    next unless b.is_win_bundle # Only get bundles that work with WSMAN
    next unless b.has_platform(parts[0], parts[1])

    if bundles[p] 
      b = bundles[p] if bundles[p].date_time > b.date_time
    end
    bundles[p] = b
  end
end

# We have the most recent bundles for each platform
# Now get the packages
data = {}
components = callback.components
try_again = true
count = 0
while try_again do
  try_again = false
  count = count + 1
  bundles.each do |p,b|
    b.contents.each do |pkg, v| 
      c = components[pkg]
      next unless c.wanted

      file = "#{files_dir}/#{pkg}"

      data[p] = {} unless data[p]
      data[p][c.package_id] = {} unless data[p][c.package_id]
      data[p][c.package_id]["file"] = file.gsub(/.*files\//, "")
      data[p][c.package_id]["type"] = "wsman"
      data[p][c.package_id]["component_type"] = c.component_type
      data[p][c.package_id]["devices"] = c.devices.values
      data[p][c.package_id]["version"] = c.vendor_version

      get_it = true
      if File.exists?(file)
        sum = %x{md5sum '#{file}'}.split(" ")[0].downcase
        get_it = sum != c.md5sum
      end

      if get_it
        puts "Getting #{file}"
        system "curl -s -L '#{c.url}' > '#{file}'"
        sum = %x{md5sum '#{file}'}.split(" ")[0].downcase
        if sum != c.md5sum
          puts "Failed to download #{pkg}"
          puts "Removing and trying again"
          %x{rm -f '#{file}'}
          try_again = true
        end
      end
    end
  end
  if count > 3 and try_again
    try_again = false
    puts "Tried 3 times to get all the packages without corruption.  Stopping"
  end
end

# Write out supported.json to files_dir
File.open("#{files_dir}/supported.json", "w") do |f|
  f.write JSON.pretty_generate(data)
end

