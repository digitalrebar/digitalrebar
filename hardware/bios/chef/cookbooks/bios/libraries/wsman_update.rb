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

require 'rubygems'
require 'xmlsimple'
require 'yaml'
require 'json'
require 'wsman'

BIOS_COMP_ID   = "159"
IDRAC_11G_COMP = "20137"
DRIVER_PACK_ID = "18981"
DIAGS_COMP_ID  = "196"
LC_COMP_ID     = "18980"

LOOKUP_STRS    = {DRIVER_PACK_ID => "Driver Pack",
                  DIAGS_COMP_ID  => "Diagnostics Package",
                  LC_COMP_ID     => "Lifecycle Controller"}

class Rebar
class BIOS

class WSMANUpdate

  attr_accessor :sysGen, :prevHash, :currHash, :jobArray

  def initialize(wsman)
    @wsman = wsman
    @prevHash  = Hash.new()
    @currHash  = Hash.new()
    @jobArray  = Array.new()
    @pruneHash = Hash.new()
    @dvKeyHash = Hash.new()
    @sysGen    = 11
    #@generation = system_generation()
  end

  def software_inventory
    puts "Enumerating software versions on node"
    output = @wsman.command("enumerate", SOFT_IDEN_URI, " -m 256")
    return false unless output

    hash = {}
    @wsman.measure_time "WSMAN enumerate inventory parse" do
      hash = XmlSimple.xml_in(output, "ForceArray" => false)
    end
    hash["Body"]["EnumerateResponse"]["Items"]["DCIM_SoftwareIdentity"]
  end

  # Example: Find iDRAC6
  # list2 = find_software_inventory_items(list, 
  #           {"ElementName" => "iDRAC.*", "ComponentType" => "FRMW", "Status" => "Installed"})
  #
  def find_software_inventory_items(inventory, test = {})
    return inventory if test.size == 0
    inventory.select do |x| 
      found = true
      test.each { |k,v| 
        found = false unless x[k] =~ /#{v}/
      }
      found
    end
  end

  def system_generation
    output = @wsman.command("enumerate", "#{WSMAN_URI_NS}/DCIM_SystemView", "-m 256")
    return 0 unless output
    hash = {}
    @wsman.measure_time "WSMAN parse SV values" do
      hash = XmlSimple.xml_in(output, "ForceArray" => false)
    end
    generation = (hash["Body"]["Fault"].nil?) ? hash["Body"]["EnumerateResponse"]["Items"]["DCIM_SystemView"]["SystemGeneration"].gsub(/[^0-9]/, "").to_i : 11
    generation
  end

  def do_update(id, uri)
    # Dump request file
    File.open("/tmp/request.xml", "w+") do |f|
      f.write %Q[
<p:InstallFromURI_INPUT xmlns:p="http://schemas.dmtf.org/wbem/wscim/1/cim-schema/2/DCIM_SoftwareInstallationService">
  <p:URI>#{uri}</p:URI>
  <p:Target xmlns:a="http://schemas.xmlsoap.org/ws/2004/08/addressing" xmlns:w="http://schemas.dmtf.org/wbem/wsman/1/wsman.xsd">
    <a:Address>http://schemas.xmlsoap.org/ws/2004/08/addressing/role/anonymous</a:Address>
    <a:ReferenceParameters>
      <w:ResourceURI>http://schemas.dmtf.org/wbem/wscim/1/cim-schema/2/DCIM_SoftwareIdentity</w:ResourceURI>
      <w:SelectorSet>
        <w:Selector Name="InstanceID">#{id}</w:Selector>
      </w:SelectorSet>
    </a:ReferenceParameters>
  </p:Target>
</p:InstallFromURI_INPUT>
]
    end
  
    # Post the update request.
    soft_svc_uri = @wsman.find_base_instance_uri(SOFT_SVC_CLASS)
    output = @wsman.command("invoke -a InstallFromURI", soft_svc_uri, " -J /tmp/request.xml")
    return false unless output

    hash = XmlSimple.xml_in(output, "ForceArray" => false)
    t = hash["Body"]["InstallFromURI_OUTPUT"]

    puts "Debug: Install URI hash = #{hash.inspect}"

    # Some versions don't actually give a return code on success.
    if (t["ReturnValue"].instance_of? Hash or t["ReturnValue"].to_i ==  RETURN_CONFIG_VAL_OK)
      jobData = @wsman.get_job_id(t["Job"])
      return true, jobData
    end

    return false, t["Message"]
  end

  #
  # update the id to the uri
  # return true,reboot on success where reboot is true or false if reboot is needed.
  # return false, message on failure.
  #
  def update(id, uri)
    answer, jid = self.do_update(id, uri)
    if answer
      # Loop until we get something interesting.
      new_count = 0
      answer, status = @wsman.get_job_status(jid)
      while answer and (status != "Downloaded" and status != "Failed" and status != "Completed")
        if status == "New"
          new_count = new_count + 1
        else
          new_count = 0
        end
        # If we are stuck in New, reboot and see if that fixes it.
        return false, true if new_count > 36
        sleep 10
        answer, status = @wsman.get_job_status(jid)
      end
      # If we completed, we are done.
      if status == "Completed"
        return true, false 
      end
      if status == "In Use"
        return true, true # Force a reboot
      end
      # If we didn't fail, we need to schedule the job and reboot.
      if status != "Failed"
        answer, status = @wsman.schedule_job(jid, "TIME_NOW")
        return answer, true
      end
      # Set error message
      jid = status
    end
    return false, jid
  end

  def match(pieces, c)
    if c["VendorID"].instance_of? String
      pieces.each do |pkg, data|
        data["devices"].each do |device|
          next unless device["vendorID"]
          next unless device["deviceID"]
  
          unless device["subDeviceID"] and device["subDeviceID"] == "" and
                 device["subVendorID"] and device["subVendorID"] == ""
            sub_truth = c["SubDeviceID"].downcase == device["subDeviceID"].downcase and
                        c["SubVendorID"].downcase == device["subVendorID"].downcase 
          else
            sub_truth = true
          end
  
          return data if c["DeviceID"].downcase == device["deviceID"].downcase and
                         c["VendorID"].downcase == device["vendorID"].downcase and
                         sub_truth
        end
      end
    else
      pieces.each do |pkg, data|
        data["devices"].each do |device|
          return data if c["ComponentID"].downcase == device["componentID"].downcase
        end
      end
    end
    nil
  end

  #
  # Assumes hash with compID => file
  # Returns list with [ [CompID, file], ...]
  #
  # Sorts - files with LC first, BIOS second, IDRAC last
  #
  def sort_updates(updates)
    return updates if updates.nil? or updates.size == 0


    ans = updates.sort do |a,b|
      af = a[1]
      bf = b[1]

      aflc = (af =~ /LC/ ? true : false)
      bflc = (bf =~ /LC/ ? true : false)
      afb = (af =~ /BIOS/ ? true : false)
      bfb = (bf =~ /BIOS/ ? true : false)
      afd = (af =~ /DRAC/ ? true : false)
      bfd = (bf =~ /DRAC/ ? true : false)

      ans = -5
      # if they are the same, compare normal
      ans = af <=> bf if (aflc and bflc) or (afb and bfb) or (afd and bfd)

      # if they are LC, they should be first
      ans = -1 if (aflc and not bflc) and ans == -5
      ans = 1 if (not aflc and bflc) and ans == -5

      # if they are BIOS, they should be next
      ans = -1 if (afb and not bfb) and ans == -5
      ans = 1 if (not afb and bfb) and ans == -5

      # if they are DRAC, they should be last
      ans = 1 if (afd and not bfd)
      ans = -1 if (not afd and bfd) and ans == -5

      # Default action
      ans = af <=> bf if ans == -5
      ans
    end

    ans
  end

  def determine_sysgen()
      puts "Powering down system..."
      @wsman.power_down_system()
      puts "Determining system generation"
      xml = @wsman.command(ENUMERATE_CMD, SYS_VIEW_URI, " -m 256")
      instanceData = @xml.processResponse(xml, '["Body"]["EnumerateResponse"]["Items"]["DCIM_SystemView"]')
      sysGenInfo = instanceData["SystemGeneration"]
      @sysGen = 12 unless (sysGenInfo.nil?)
  end

  def formulate_key(vendorId,devId,subVenId,subDevId)
    retStr = ""
    retStr += vendorId.downcase       unless vendorId.is_a?(Hash)  or vendorId.nil?
    retStr += ":" + devId.downcase    unless devId .is_a?(Hash)    or devId .nil?
    retStr += ":" + subVenId.downcase unless subVenId .is_a?(Hash) or subVenId .nil?
    retStr += ":" + subDevId.downcase unless subDevId .is_a?(Hash) or subDevId .nil?
    retStr
  end

  ## Remove dups that have no matching target device and construct pruned hash
  ## with matches based on device id associations
  def prune_previous_hash()
    currHashKeys = @currHash.keys
    @prevHash.each do |newKey, instance|
      ## implies all fields match (either component id or
      ## the key containing device id, vendor id, sub device id etc
      if (@currHash.has_key?(newKey))
        @pruneHash[newKey] = @prevHash[newKey]
      ## the previous hash contains keys that have only the device and
      ## vendor id sometimes...so need to match the keys on the same
      ## basis
      elsif (@dvKeyHash.has_key?(newKey))
        tempKey = newKey + @dvKeyHash[newKey][0]
        @pruneHash[tempKey] = @prevHash[newKey]
      end
    end
  end

  def get_identity_instances()
    puts "Enumerating 11G software identity instances"
    begin
      xml = @wsman.command(ENUMERATE_CMD, SOFT_IDEN_URI, " -m 512 -M objepr")
      instanceList = @wsman.processResponse(xml, '["Body"]["EnumerateResponse"]["Items"]["Item"]')
      instanceList = (instanceList.instance_of?(Array))?instanceList:[instanceList]
      instanceList.each do |instance|
        compId = instance['DCIM_SoftwareIdentity']['ComponentID']
        instID = instance['DCIM_SoftwareIdentity']['InstanceID']
        verStr = (instance['DCIM_SoftwareIdentity']['VersionString']).strip
        elName = instance['DCIM_SoftwareIdentity']['ElementName']
        if (LOOKUP_STRS.has_key?(compId))
          tempStr = LOOKUP_STRS[compId]
          puts "No downgrade for #{tempStr}...Skipping"
        elsif (elName =~ /power\s*supply.*/i)
          puts "No downgrade for power supply...Skipping"
        elsif (compId == BIOS_COMP_ID or compId == IDRAC_11G_COMP)
          if (instID =~ /DCIM:INSTALLED.*/i)
            @currHash[compId] = instance
          elsif (instID =~ /DCIM:AVAILABLE.*/i)
            if (@prevHash.has_key?(compId))
              prevVer = (@prevHash[compId]['DCIM_SoftwareIdentity']['VersionString']).strip
              if ((verStr <=> prevVer) == 0)
                puts "Available versions match..."
              elsif ((verStr <=> prevVer) == -1)
                #puts "#{verStr} < #{prevVer}...Replacing in hash"
                @prevHash[compId] = instance
              #else
                #puts "Current version > prev version in hash...skipping"
              end
            else
              @prevHash[compId] = instance
            end
          else
            puts "Not handling version for hash insertion .. #{instID}"
          end
        elsif (compId.is_a?(Hash))
          deviceId    = instance['DCIM_SoftwareIdentity']['DeviceID']
          subDeviceId = instance['DCIM_SoftwareIdentity']['SubDeviceID']
          vendorId    = instance['DCIM_SoftwareIdentity']['VendorID']
          subVendorId = instance['DCIM_SoftwareIdentity']['SubVendorID']
          newKey = formulate_key(vendorId,deviceId,subVendorId,subDeviceId)
          ## Each port on a NIC will show up as a separate device..but for all
          ## of them one update job is all that is required..
          if (instID =~ /DCIM:INSTALLED.*/i and !@currHash.has_key?(newKey))
            @currHash[newKey] = instance
            tempKey = vendorId.downcase + ":" + deviceId.downcase
            tempVal = ":" + subVendorId.downcase + ":" + subDeviceId.downcase
            if (@dvKeyHash.has_key?(tempKey))
              @dvKeyHash[tempKey].push(tempVal)
            else
              @dvKeyHash[tempKey] = Array.new if (@dvKeyHash[tempKey].nil?)
              @dvKeyHash[tempKey].push(tempVal)
            end
          elsif (instID =~ /DCIM:AVAILABLE.*/i)
            if (@prevHash.has_key?(newKey))
              prevVer = (@prevHash[newKey]['DCIM_SoftwareIdentity']['VersionString']).strip
              if ((verStr <=> prevVer) == 0)
                puts "Available versions match...skipping hash op"
              elsif ((verStr <=> prevVer) == -1)
                @prevHash[newKey] = instance
              else
                puts "Current version > prev version in hash...skipping"
              end
            else
              @prevHash[newKey] = instance
            end
          end
        end
      end
      prune_previous_hash()
      @prevHash = @pruneHash
      retVal = setup_firmware_downgrades()
      if (retVal)
        # at least one downgrade was initiated
        retVal,jobID = @wsman.create_update_reboot_job()
        if (retVal)
          @jobArray.push(jobID)
          puts "Setting up job queue..."
          retVal = @wsman.setup_job_queue_multi(@jobArray)
          if (retVal)
            @wsman.poll_multiple_jobs(@jobArray)
          else
            puts "Failed to set up job queue...exiting"
          end
        else
          puts "Failed reboot job creation..exiting"
        end
      end
    rescue Exception => e
      puts "Caught Exception...#{e.message}"
    end
  end

  def get_12g_identity_instances()
    puts "Enumerating software identity instances for 12G system..."
    begin
      xml = @wsman.command(ENUMERATE_CMD, SOFT_IDEN_URI, " -m 512 -M objepr")
      instanceList = @wsman.processResponse(xml, '["Body"]["EnumerateResponse"]["Items"]["Item"]')
      instanceList = (instanceList.instance_of?(Array))?instanceList:[instanceList]
      instanceList.each do |instance|
        fqdd = instance['DCIM_SoftwareIdentity']['FQDD']
        instID = instance['DCIM_SoftwareIdentity']['InstanceID']
        if (instID =~ /DCIM:INSTALLED.*/i)
          @currHash[fqdd] = instance
        elsif (instID =~ /DCIM:PREVIOUS.*/i)
          @prevHash[fqdd] = instance
        end
      end
      retVal = setup_firmware_downgrades()
      if (retVal)
        retVal,jobID = @wsman.create_update_reboot_job()
        if (retVal)
          @jobArray.push(jobID)
          retVal = @wsman.setup_job_queue_multi(@jobArray)
          if (retVal)
            @wsman.poll_multiple_jobs(@jobArray)
          else
            puts "Failed to set up job queue...exiting"
          end
        else
          puts "Failed reboot job creation..exiting"
        end
      end
    rescue Exception => e
      puts "Caught exception...#{e.message}"
    end
  end

  def install_from_software_identity(soapEnv)
    uriStr = @wsman.find_instance_uri(SOFT_SVC_CLASS)
    cmd = "InstallFromSoftwareIdentity"
    inputFile = "/tmp/downgrade.xml"
    File.open("#{inputFile}", "w+") do |f|
      f.write %Q[#{soapEnv}]
    end
    output = @wsman.command("#{INVOKE_CMD} -a #{cmd}", uriStr, "-J #{inputFile}")
    retVal = @wsman.returnValue(output,cmd)
    if (retVal.to_i == RETURN_CFG_OK)
      puts "No JID returned...invocation of downgrade complete"
    elsif (retVal.to_i == RETURN_CONFIG_VAL_OK)
      wsInstance = @wsman.processResponse(output, '["Body"]["InstallFromSoftwareIdentity_OUTPUT"]["Job"]')
      jobID = @wsman.get_job_id(wsInstance)
      @jobArray.push(jobID)
    else
      puts "Error encountered in job creation..."
    end
  end

  def setup_firmware_downgrades()
    retVal  = false
    currLen = @currHash.length
    prevLen = @prevHash.length
    prevHash.each do |fqdd, instance|
      #puts fqdd
      if (currHash.has_key?(fqdd))
        prevVer = (instance['DCIM_SoftwareIdentity']['VersionString']).strip
        currVer = (currHash[fqdd]['DCIM_SoftwareIdentity']['VersionString']).strip
        elName  = instance['DCIM_SoftwareIdentity']['ElementName']
        if ( (prevVer <=> currVer) == -1 )
          puts "Downgrading #{fqdd}::#{elName} from #{currVer} to #{prevVer}"
          initiate_downgrade(instance)
          retVal = true
        elsif ((prevVer <=> currVer) == 1)
          puts "No downgrade needed  #{fqdd}::#{elName} from #{currVer} to #{prevVer}"
          #initiate_downgrade(currHash[fqdd])
        else
          puts "Version match...not downgrading component #{fqdd}:#{elName}"
        end
      end
    end
    retVal
  end

  def formulate_soap_envelope(selectorStr)
    retStr = %Q[<p:InstallFromSoftwareIdentity_INPUT xmlns:p="http://schemas.dmtf.org/wbem/wscim/1/cim-schema/2/root/dcim/DCIM_SoftwareInstallationService">
                <p:Target xmlns:a="http://schemas.xmlsoap.org/ws/2004/08/addressing" xmlns:w="http://schemas.dmtf.org/wbem/wsman/1/wsman.xsd">
                <a:Address>http://schemas.xmlsoap.org/ws/2004/08/addressing/role/anonymous</a:Address>
                 <a:ReferenceParameters>
                   <w:ResourceURI>http://schemas.dell.com/wbem/wscim/1/cim-schema/2/DCIM_SoftwareIdentity</w:ResourceURI>
                   <w:SelectorSet>
                    #{selectorStr}
                   </w:SelectorSet>
                 </a:ReferenceParameters>
                </p:Target>
</p:InstallFromSoftwareIdentity_INPUT>]
  end

  def initiate_downgrade(instanceInfo)
    selStr = @wsman.get_selector_string(instanceInfo)
    soapEnv = formulate_soap_envelope(selStr)
    install_from_software_identity(soapEnv)
  end




end

end
end

#######################################################

#
# Assumes that supported.json has been returned
#
def test_update(opts)
  require 'wsman'
  sys = %x{dmidecode -t system | grep "Product Name:" | awk -F: '{ print $2 }'}.strip!
  puts "System: #{sys}"

  system("wget -q http://#{opts[:prov_ip]}:#{opts[:prov_port]}/files/wsman/supported.json -O /tmp/supported.json")
  jsondata = File.read('/tmp/supported.json')
  data = JSON.parse(jsondata)
  unless data
    puts "Failed to load the supported file"
    exit 1
  end

  pieces = data[sys]
  unless pieces
    puts "Failed to find sys"
    exit 1
  end
  wsman = Rebar::WSMAN.new(opts)
  wsman_update = Rebar::BIOS::WSMANUpdate.new(wsman)

  list = wsman_update.software_inventory
  list2 = wsman_update.find_software_inventory_items(list, {"Status" => "Installed"})

  updates = {}
  list2.each do |c|
    if k = wsman_update.match(pieces, c)
      if c["VersionString"] == k["version"]
        puts "Already at correct version: #{c["ElementName"]}"
        next
      end

      updates[c["InstanceID"]] = k["file"]
    else
      puts "No update for #{c["ElementName"]} #{c["ComponentID"]}"
    end
  end

  # Sort the updates.
  updates = wsman_update.sort_updates(updates)

  # Clear jobs
  answer, status = wsman.clear_all_jobs
  if !answer
    return false, status
  end

  # Do the updates
  updates.each do |d|
    id = d[0]
    file = d[1]
    puts "Update: ID: #{id},   FILE:#{file}"
    answer, jid = wsman_update.update(id, "http://#{opts[:prov_ip]}:#{opts[:prov_port]}/files/#{file}")
    if answer
      puts "Job scheduled ready for reboot" if jid
      puts "Job completed" unless jid
    else
      puts "update failed: #{jid}"
    end
  end

end
