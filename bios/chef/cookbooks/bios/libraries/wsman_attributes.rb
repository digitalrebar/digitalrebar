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
require "xmlsimple"
require "xml"
require "json"
require "wsman"

CHANGE_BOOT_ORDER_CMD = "ChangeBootOrderByInstanceID"

class Crowbar
  class BIOS

class WSMANAttributes
  def initialize(wsman)
    @wsman = wsman
  end

#### Structs

  class Item < Hash
    def initialize(type)
      self["Type"] = type
    end
    
    def type
      self["Type"]
    end

    def is_string
      self["Type"] =~ /.*String/
    end
    def is_integer
      self["Type"] =~ /.*Integer/
    end
    def is_enumeration
      self["Type"] =~ /.*Enumeration/
    end
    def is_password
      self["Type"] =~ /.*Password/
    end
    def is_bios
      self["Type"] =~ /^DCIM_BIOS.*/
    end
    def is_raid
      self["Type"] =~ /^DCIM_RAID.*/
    end
    def is_idrac
      self["Type"] =~ /^DCIM_iDRAC.*/
    end
    def is_nic
      self["Type"] =~ /^DCIM_NIC.*/
    end

    def fqdd
      self["FQDD"]
    end
    def group_id
      self["GroupID"]
    end
    def attribute_name
      self["AttributeName"]
    end
    def instance_id
      self["InstanceID"]
    end
    def possible_values
      self["PossibleValues"]
    end
    def max_length
      self["MaxLength"].to_i
    end
    def min_length
      self["MinLength"].to_i
    end
    def lower_bound
      self["LowerBound"].to_i
    end
    def upper_bound
      self["UpperBound"].to_i
    end
    def is_read_only
      (self["IsReadOnly"] == "true") or (self["IsReadOnly"] == "True")
    end
    def current_value
      self["CurrentValue"]
    end
    def default_value
      self["DefaultValue"]
    end
    def pending_value
      self["PendingValue"]
    end
  end

#### Callback

  class AttributeCallbacks
    include XML::SaxParser::Callbacks

    attr_reader :items
    attr_reader :fault

    def initialize 
      @ignore_list = [ "s:Header", "s:Envelope", "s:Body", "wsa:To", "wsa:Action", "wsa:RelatesTo", "wsa:MessageID",
                       "wsen:EnumerateResponse", "wsman:Items", "wsen:EnumerationContext", "wsman:EndOfSequence" ]
      @leaf_list = [ "AttributeName", "CurrentValue", "DefaultValue", "FQDD", "InstanceID", "IsReadOnly", "LowerBound",
                     "PendingValue", "UpperBound", "MinLength", "MaxLength", "GroupID", "GroupDisplayName", 
                     "AttributeDisplayName", "Dependency", "DisplayOrder",
                     "ElementName", "Description", "Caption", "IsOrderedList",
                     "PossibleValuesDescription", "StringType", "ValueExpression",
                     "PossibleValues", "Code", "Value", "Subcode", "Reason", "Detail", "FaultDetail", "Text" ]
      @item_list = [ "Fault", "DCIM_BIOSInteger", "DCIM_BIOSEnumeration", "DCIM_BIOSString", 
                     "DCIM_NICInteger", "DCIM_NICString", "DCIM_NICEnumeration",
                     "DCIM_RAIDInteger", "DCIM_RAIDString", "DCIM_RAIDEnumeration",
                     "DCIM_LCInteger", "DCIM_LCString", "DCIM_LCEnumeration",
                     "DCIM_iDRACCardString", "DCIM_iDRACCardInteger", "DCIM_iDRACCardEnumeration" ]
      @stack = []
      @items = []
      @fault = nil
      @current = nil
    end

    def is_ignore_element?(element)
      @ignore_list.include?(element)
    end

    def is_leaf_element?(element)
      @leaf_list.include?(element)
    end

    def is_item_element?(element)
      @item_list.include?(element)
    end

    def on_end_element(element)
      return if is_ignore_element? element

      element = element.gsub!(/.*:/, "")
      puts "Baddness" if @stack.pop != element

      if is_item_element? element
        @items << @current unless element == "Fault"
        @current = nil
        return
      end

      # This is a list of values
      if element == "PossibleValues"
        arr = @current["PossibleValues"]
        arr = [] unless arr
        arr << @buffer
        @current["PossibleValues"] = arr
        return
      end

      return if element == "Code" # Handled by containing Value
      return if element == "Subcode" # Handled by containing Value
      return if element == "Reason" # Handled by containing Value

      if is_leaf_element? element
        element = @stack.last if ((element == "Value") or (element == "Text"))
        @current[element] = @buffer unless @element_is_nil
        @current[element] = "NIL" if @element_is_nil
        return
      end

      puts "#Element ended: #{element} #{@buffer}"
    end

    def on_cdata_block(msg)
      @buffer = @buffer + msg
      @buffer = @buffer.strip
    end

    def on_characters(chars)
      @buffer = @buffer + chars
      @buffer = @buffer.strip
    end

    def on_start_element(element, attributes)
      @buffer=""
      return if is_ignore_element? element

      element = element.gsub!(/.*:/, "")
      @stack.push(element)

      if is_item_element? element
        @current = Item.new(element)
        @fault = @current if element == "Fault"
        return
      end

      if is_leaf_element? element
        @element_is_nil = attributes["xsi:nil"] == "true"
        return
      end

      puts "#Element started: #{element}"
    end
  end

  def attributes(section, debug=false)
    output = @wsman.command("enumerate", "#{WSMAN_URI_NS}/#{section}", "-m 512 -V")
    return false unless output

    puts "#{output}" if debug

    callback = AttributeCallbacks.new
    @wsman.measure_time "WSMAN enumerate attributes #{section} SAX" do
      parser = XML::SaxParser.string(output)
      parser.callbacks = callback
      parser.parse
    end

    return callback.fault if callback.fault
    return callback.items
  end

  #
  # Get the attributes and save them into the node
  # return the attrs.
  #
  def record_attributes(node)
    attrs = []
    h = self.attributes("DCIM_BIOSEnumeration")
    attrs << h
    h = self.attributes("DCIM_BIOSString")
    attrs << h
    h = self.attributes("DCIM_BIOSInteger")
    attrs << h
    h = self.attributes("DCIM_NICAttribute")
    attrs << h
    h = self.attributes("DCIM_iDRACCardString")
    attrs << h
    h = self.attributes("DCIM_iDRACCardInteger")
    attrs << h
    h = self.attributes("DCIM_iDRACCardEnumeration")
    attrs << h
    h = self.attributes("DCIM_RAIDAttribute")
    attrs << h
    h = self.attributes("DCIM_LCAttribute")
    attrs << h
    attrs = attrs.flatten!

    node["crowbar_wall"] = {} unless node["crowbar_wall"]
    node["crowbar_wall"]["bios"] = {} unless node["crowbar_wall"]["bios"]
    node["crowbar_wall"]["bios"]["settings"] = {} unless node["crowbar_wall"]["bios"]["settings"]
    hash = node["crowbar_wall"]["bios"]["settings"]
    attrs.each do | item |
      hash[item.instance_id] = item.current_value
    end
    node.save

    attrs
  end

  #
  # Set a set of attributes in a section of the system by name
  # Get the service access information
  # Build input.xml
  # Post WSMAN section action
  #
  # Assumes that caller will setup the reboot job.
  #
  def set_attributes(fqdd, pairs)
    case fqdd.split(/[.:]/)[0]
    when "BIOS"
      class_name = "DCIM_BIOSService"
    when "iDRAC"
      class_name = "DCIM_iDRACCardService"
    when "NIC"
      class_name = "DCIM_NICService"
    when "LC"
      class_name = "DCIM_LCService"
    when "RAID"
      class_name = "DCIM_RAIDService"
    when "Enclosure"
      class_name = "DCIM_RAIDService"
    end

    svc_class_uri = @wsman.find_instance_uri(class_name)
    res = "#{WSMAN_URI_NS}/#{class_name}"

    reboot = false
    method = "SetAttribute"
    method = "SetAttributes" if pairs.size > 1
    method = "ApplyAttributes" if class_name == "DCIM_iDRACCardService"

    if (class_name != "DCIM_LCService")
      output = @wsman.command("invoke -a DeletePendingConfiguration -k Target=#{fqdd}", svc_class_uri)
      returnVal = @wsman.returnValue(output,"DeletePendingConfiguration")
      if returnVal.to_i == RETURN_CFG_OK
        puts "SET_ATTR:Successfully cleared pending configuration on #{fqdd}"
      else
        puts "SET_ATTR:Either no pending config or error deleting pending config on #{fqdd}"
      end
    end

    # Dump request file
    File.open("/tmp/request.xml", "w+") do |f|
      f.write %Q[ <p:#{method}_INPUT xmlns:p="#{res}"> ]
      f.write %Q[ <p:Target>#{fqdd}</p:Target> ] unless class_name == "DCIM_LCService"
      pairs.each do |k,v|
        f.write %Q[ <p:AttributeName>#{k}</p:AttributeName> ]
        f.write %Q[ <p:AttributeValue>#{v}</p:AttributeValue> ]
      end
      f.write %Q[ </p:#{method}_INPUT> ]
    end

    puts "Debug: set attribute request.xml"
    puts %x{cat /tmp/request.xml}

    # Post the attribute update request.
    output = @wsman.command("invoke -a #{method}",svc_class_uri, "-J /tmp/request.xml")
    puts "Debug: set attr failed no output" unless output 
    return [ false, "Failed to update attributes" ] unless output 

    hash = XmlSimple.xml_in(output, "ForceArray" => false)
    t = hash["Body"]["#{method}_OUTPUT"] rescue nil

    puts "Debug: set attr hash = #{hash.inspect}"

    if class_name == "DCIM_iDRACCardService"
      if t.nil? or t["ReturnValue"].to_i != 4096
        puts "Set Attr (iDrac): return failed: #{(t.nil? ? "Unknown" : t["Message"])}" 
        return [ false, (t.nil? ? "Unknown" : t["Message"]) ]
      end
      return [ true, false ]
    end

    if t.nil? or t["ReturnValue"].to_i != 0
      puts "Set Attr: return failed: #{(t.nil? ? "Unknown" : t["Message"])}" 
      return [ false, (t.nil? ? "Unknown" : t["Message"]) ]
    end

    reboot = reboot || (t["RebootRequired"] == "Yes")
 
    ## With new set_boot restructuring it's possible we could hit the   ##
    ## BIOS barclamp and reboot into UEFI mode without hitting the set  ##
    ## boot recipe...To prevent this we do a minimal reordering of UEFI ##
    ## boot sources if the bios attributes set the pending mode to UEFI ##
    ## The config job is created by the calling method..                ##
    if (class_name == "DCIM_BIOSService")
      rebootFlag = false
      return_val, rebootFlag = @wsman.check_and_handle_boot_sources()
      #override what the method returned...basically nothing may have happened
      # in the check and handle boot sources method...but we still need to reboot
      # to set the attributes
      reboot = true if (!reboot and rebootFlag)  
    end
    ## End boot manipulation hack

    if class_name == "DCIM_LCService"
      # Post Targeted Config Job
      method = "CreateConfigJob"
      output = @wsman.command("invoke -a #{method}",svc_class_uri,"")
      puts "Debug: set attr LC job failed no output" unless output 
      return [ false, "Failed to create LC update job" ] unless output 

      hash = XmlSimple.xml_in(output, "ForceArray" => false)
      t = hash["Body"]["#{method}_OUTPUT"]

      puts "Debug: config LC job hash = #{hash.inspect}"

      if t["ReturnValue"].to_i != 4096
        puts "Set Attr: return failed LC config job: #{(t.nil? ? "Unknown" : t["Message"])}" 
        return [ false, t["Message"] ]
      end
      # Always reboot on LC changes
      reboot = true
    else
      # Post Targeted Config Job
      method = "CreateTargetedConfigJob"
      output = @wsman.command("invoke -a #{method}", svc_class_uri,"-k Target=#{fqdd} -k ScheduledStartTime=\"TIME_NOW\"")
      puts "Debug: set attr job failed no output" unless output 
      return [ false, "Failed to create update job" ] unless output 

      hash = XmlSimple.xml_in(output, "ForceArray" => false)
      t = hash["Body"]["#{method}_OUTPUT"]

      puts "Debug: config job hash = #{hash.inspect}"

      if t["ReturnValue"].to_i != 4096
        puts "Set Attr: return failed config job: #{(t.nil? ? "Unknown" : t["Message"])}" 
        return [ false, t["Message"] ]
      else
        job_id = @wsman.get_job_id(t["Job"])
        puts "Targeted config job ID is #{job_id}"
      end
    end
    return [ true, reboot ]
  end

  # Helper function to handle wildcarding in the attributes file
  def get_new_value(item, attributes)
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
    return nil unless fqdd_part

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
    return nil unless group_part

    # Return value if it exists.
    return nil unless group_part[attr_name]
    group_part[attr_name]["value"]
  end

  def log_action(action, node)
    node["crowbar_wall"] = {} unless node["crowbar_wall"]
    node["crowbar_wall"]["bios"] = {} unless node["crowbar_wall"]["bios"]
    node["crowbar_wall"]["bios"]["actions"] = [] unless node["crowbar_wall"]["bios"]["actions"]
    node["crowbar_wall"]["bios"]["actions"] << action
  end

  #
  # Update attributes
  #
  # First save attributes off into node
  # Use the returned structure to difference what needs to be set.
  # if work needs to be done, set the attributes and reboot.
  #
  def update_attributes(attributes, opts = {})
    node = opts[:node]
    attrs = record_attributes(node)

    set_something = false
    error = false
    content_hash = Hash.new
    set_something = false
    attrs.each do | item |
      next if item.is_read_only  # Skip read only vars

      new_val = get_new_value(item, attributes)
      next unless new_val # Skip items that don't have a new value

      if item.is_string
        return [ false, "Invalid #{new_val} for #{item.instance_id}" ] if ((new_val.length > item.max_length) or (new_val.length < item.min_length))
      elsif item.is_integer
        a = new_val.to_i rescue "Failed"
        return [ false, "Not an number (#{new_val}) for #{item.instance_id}" ] if a == "Failed"
        return [ false, "Invalid #{new_val} for #{item.instance_id}" ] if ((a > item.upper_bound) or (a < item.lower_bound))
      elsif item.is_enumeration
        return [ false, "Invalid #{new_val} for #{item.instance_id}" ] unless item.possible_values.include? new_val
      else
        return [ false, "Unknown type of item" ]
      end

      if new_val != item.current_value
        set_something = true

        fqdd = item.fqdd ? item.fqdd : "LC"
        group_id = item.group_id
        group_id = nil if group_id and not item.instance_id.include?("#{group_id}#")
        attr_name = item.attribute_name
        attr_name = "#{group_id}##{attr_name}" if group_id
        content_hash[fqdd] = {} unless content_hash[fqdd]
        content_hash[fqdd][attr_name] = new_val
        puts "Setting #{attr_name} to #{new_val} from #{item.current_value} in #{fqdd}"
        log_action("Setting #{attr_name} to #{new_val} from #{item.current_value} in #{fqdd}", node)
      end
    end

    if set_something and !error
      reboot = false
      content_hash.each do |fqdd, pairs|
        res, reb = self.set_attributes(fqdd, pairs)
        reboot = (reboot || reb) if res
        error = (error || !res)
      end

      # If we didn't error and didn't reboot, we need to
      # record the attributes again.  We may not comeback here.
      # If we are rebooting, then we are fine.
      record_attributes(node) if !error and !reboot
    end
    [!error, reboot]
  end

end # WSMAN_Attributes

end # BIOS
end # Crowbar

#######################################################

def test_build_config_json(opts)
  wsman = Crowbar::WSMAN.new(opts)
  wsman_attributes = Crowbar::BIOS::WSMANAttributes.new(wsman)
  attrs = []
  h = wsman_attributes.attributes("DCIM_BIOSEnumeration")
  attrs << h
  h = wsman_attributes.attributes("DCIM_BIOSString")
  attrs << h
  h = wsman_attributes.attributes("DCIM_BIOSInteger")
  attrs << h
  h = wsman_attributes.attributes("DCIM_NICAttribute")
  attrs << h
  h = wsman_attributes.attributes("DCIM_iDRACCardAttribute")
  attrs << h
  h = wsman_attributes.attributes("DCIM_RAIDAttribute")
  attrs << h
  h = wsman_attributes.attributes("DCIM_LCAttribute")
  attrs << h

  attrs = attrs.flatten!

  json = Hash.new
  json["id"] = "bios-set-PowerEdgeR710-default"
  json["style"] = "wsman"
  json["attributes"] = Hash.new
  attrs.each do | item |
    fqdd = item.fqdd ? item.fqdd : "LC"
    fqdd = fqdd.split(/[.:]/)[0]
    json["attributes"][fqdd] = {} unless json["attributes"][fqdd]
    group_id = item.group_id
    group_id = nil if group_id and not item.instance_id.include?("#{group_id}#")
    group_id = group_id ? group_id : "default"
    group_id = group_id.split(/[.:]/)[0]
    json["attributes"][fqdd][group_id] = {} unless json["attributes"][fqdd][group_id]
    if json["attributes"][fqdd][group_id][item.attribute_name]
      puts "Delete #{fqdd}.#{group_id}.#{item.attribute_name}" if json["attributes"][fqdd][group_id][item.attribute_name]["value"] != item.current_value
    end
    json["attributes"][fqdd][group_id][item.attribute_name] = { 
      "instance_id" => item.instance_id, 
      "value" => item.current_value, 
      "attr_name" => item.attribute_name, 
      "GroupID" => item.group_id, 
      "DefaultValue" => item.default_value
    }
    # out = "#{item.instance_id} #{item.type} #{item.is_read_only} \"#{item.current_value}\" "
    # out = out + "#{item.min_length} #{item.max_length}" if item.is_string
    # out = out + "#{item.lower_bound} #{item.upper_bound}" if item.is_integer
    # out = out + "#{item.possible_values.join(",")} " if item.is_enumeration
    # puts out
  end

  puts JSON.pretty_generate(json)
end

def test_set_attributes(attrs, opts)
  wsman = Crowbar::WSMAN.new(opts)
  wsman_attributes = Crowbar::BIOS::WSMANAttributes.new(wsman)

  node = {}
  ret, message = wsman_attributes.update_attributes(attrs, { :node => node })
  [ ret, message, node ]
end


