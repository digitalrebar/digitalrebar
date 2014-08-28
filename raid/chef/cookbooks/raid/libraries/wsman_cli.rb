#!/c/Ruby187/bin/ruby
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
require 'pp'
require 'wsman'
 
require File.join(File.dirname(__FILE__), 'raid_data')

ENUMERATE_CMD = 'enumerate'
INVOKE_CMD = 'invoke' 
GET_CMD = 'get'
CONVERT_TO_RAID_CMD = "ConvertToRAID" 
RESET_CONF_CMD      = "ResetConfig" 
CLEAR_FOREIGN_CMD   = "ClearForeignConfig"
GET_RAID_CMD        = "GetRAIDLevels"
CHANGE_BOOT_ORDER_CMD = "ChangeBootOrderByInstanceID"

INVOKE_BOOT_CONFIG_URI =  "#{WSMAN_URI_NS}/DCIM_BootConfigSetting?InstanceID="

LIFECYCLE_JOB_URI = "#{WSMAN_URI_NS}/DCIM_LifecycleJob"

LC_STATUS_READY="Ready"

CSIOR_ATTR="Collect System Inventory on Restart"
CSIOR_ATTR_URI = "#{WSMAN_URI_NS}/DCIM_LCEnumeration?InstanceID=DCIM_LCEnumeration:CCR5"

SPAN_LENGTH = 2

RAID0_VAL  = '2'
RAID1_VAL  = '4'
RAID10_VAL = '2048'
RAID5_VAL  = '64'
RAID6_VAL  = '128'
RAID50_VAL = '8192'
RAID60_VAL = '16384'

RAID10_SPAN_TABLE = {
  4 => { :span_depth => 2, :span_length => 2 },
  6 => { :span_depth => 3, :span_length => 2 },
  8 => { :span_depth => 4, :span_length => 2 },
 10 => { :span_depth => 5, :span_length => 2 },
 12 => { :span_depth => 6, :span_length => 2 },
 14 => { :span_depth => 7, :span_length => 2 },
 16 => { :span_depth => 8, :span_length => 2 },
 18 => { :span_depth => 3, :span_length => 6 },
 20 => { :span_depth => 5, :span_length => 4 },
 22 => { :span_depth => 5, :span_length => 4, :drop_count => 2 }, # EEK!!
 24 => { :span_depth => 6, :span_length => 4 },
 26 => { :span_depth => 6, :span_length => 4, :drop_count => 2 }, # EEK!!
 28 => { :span_depth => 7, :span_length => 2 },
 30 => { :span_depth => 5, :span_length => 6 },
 32 => { :span_depth => 8, :span_length => 4 }
}

class Crowbar
  class RAID
    class WsManCli < Crowbar::RAID::Driver
      
     
      attr_accessor :raid_svc_uri

      VD_STATUS_MAP = {
                        "0" => "Unknown",
                        "1" => "Ready",
                        "2" => "Online",
                        "3" => "Foreign",
                        "4" => "Offline",
                        "5" => "Blocked",
                        "6" => "Failed",
                        "7" => "Degraded",
                        "8" => "Non-RAID"
                     }

      RAID_LEVEL_MAP = {
                         "1"     => :JBOD,
                         "2"     => :RAID0,
                         "4"     => :RAID1,
                         "64"    => :RAID5,
                         "128"   => :RAID6,
                         "2048"  => :RAID10,
                         "8192"  => :RAID50,
                         "16384" => :RAID60
                      }

      CTRL_STATUS_MAP = {
                          "0"      => "Unknown",
                          "1"      => "OK",
                          "2"      => "Degraded",
                          "3"      => "Error",
                          "0x8000" => "DMTF Reserved",
                          "0xFFFF" => "Vendor Reserved"
                        }

      BUS_PROTO_MAP = {
                        "0" => :Unknown,
                        "1" => :SCSI,
                        "2" => :PATA,
                        "3" => :FIBRE,
                        "4" => :USB,
                        "5" => :SATA,
                        "6" => :SAS
                      }

      MEDIA_TYPE_MAP = {
                         "0" => :HDD,
                         "1" => :SSD
                       }

      RAID_ENUM_MAP = {
                         "RAID-0"   => :RAID0,
                         "RAID-1"   => :RAID1,
                         "RAID-5"   => :RAID5,
                         "RAID-6"   => :RAID6,
                         "RAID-10"  => :RAID10,
                         "RAID-50"  => :RAID50,
                         "RAID-60"  => :RAID60
                      }

      def initialize(node)
        require 'wsman'
        $in_chef = true
        user = node[:ipmi][:bmc_user] if (node[:ipmi] and node[:ipmi][:bmc_user])
        password = node[:ipmi][:bmc_password] if (node[:ipmi] and node[:ipmi][:bmc_password])
        host = node["crowbar_wall"]["ipmi"]["address"] if (node["crowbar_wall"] and node["crowbar_wall"]["ipmi"] and node ["crowbar_wall"]["ipmi"]["address"] )
        if (host)
          puts "BMC/iDRAC IP is #{host}"
        else
          puts("BMC/iDRAC IP is null")
        end
        opts = { :user => user, :password => password, :host => host, :port => 443, :debug_time => true }
        if (host)
          wsman = Crowbar::WSMAN.new(opts) 
          @wsman = wsman
        else
          @wsman = nil
          log("Setting wsman object to Nil", :DEBUG)
        end
        @xml = XML_UTIL.new
        @node = node
        @raid_svc_uri = nil
      end

      def wait_until_lc_ready()
        ctr = 0
        ready = false
        while (!ready and ctr < 4)
          ready, value = @wsman.is_RS_ready?
          ctr += 1
          ## Wait 30 seconds before pinging idrac again
          ## if the system is not ready...else move on
          if (!ready)
            puts "LC not ready for commands...Retrying after 30s"
            sleep 30
          end
        end 
      end
      
      def job_status(job_id)
        log("job_status, job_id: #{job_id}.")
        url = "#{LIFECYCLE_JOB_URI}?InstanceID=#{job_id}"
        xml = @wsman.command(GET_CMD, url)
        percent_complete = @xml.processResponse(xml,'["Body"]["DCIM_LifecycleJob"]["PercentComplete"]')
        percent_complete
      end

      #
      # All unallocated disks are in the jbod volume
      #
      def fix_up_jbod(c)
        disks = c.disks.dup
        jbod_disks = []

        # Raid 0 with a single Drive is JBOD.
        # Remove from volumes and add to list
        del = []
        c.volumes.each do |v|
          del << v if v.raid_level == :RAID0 and v.members.length == 1
        end
        del.each do |v|
         # Save off the vol id on the disk
         v.members[0].vol_id = v.vol_id
         jbod_disks << v.members[0]
         c.volumes.delete(v)
        end

        unless jbod_disks.empty?
          rv = Crowbar::RAID::Volume.new
          rv.controller = c
          rv.raid_level = :JBOD
          rv.members = jbod_disks
          c.volumes << rv
        end
      end

      def parse_cntr_info(raw_cntr)
         c = Controller.new(:controller_id          => raw_cntr['FQDD'],
                            :vendor_id              => raw_cntr['PCIVendorID'],
                            :sub_vendor_id          => raw_cntr['PCISubVendorID'],
                            :device_id              => raw_cntr['PCIDeviceID'],
                            :sub_device_id          => raw_cntr['PCISubDeviceID'],
                            :pci_slot               => raw_cntr['PCISlot'],
                            :bus                    => raw_cntr['Bus'],
                            :device                 => raw_cntr['Device'],
                            :function               => raw_cntr['Function'],
                            :firmware_version       => raw_cntr['ControllerFirmwareVersion'],
                            :supported_raid_levels  => Array.new,
                            :has_non_raid_disks     => false,
                            :product_name           => raw_cntr['ProductName'])
         c
      end

      def enumerate_topology
        
        @controllers = []
        controllerList = get_controllers()

        ## Delete the pending config on controllers to ensure VDs marked for
        ## deletion / VDs marked for creation ( but uncommitted) do not show up
        ## and mess up our topology tree
        delete_pending_config_on_controllers(controllerList)

        phyDiskList    = physical_disks()
        virtDiskList   = virtual_disks(phyDiskList)
        if (controllerList and controllerList.length > 0)
            controllerList.each do |cntrlr|
              c         = parse_cntr_info(cntrlr)
              get_all_possible_raid_levels(c)
              c.driver  = self
              c.disks   = parse_assoc_dev_info(c,phyDiskList) if (phyDiskList and phyDiskList.length > 0)
              c.volumes = parse_assoc_dev_info(c,virtDiskList) if (virtDiskList and virtDiskList.length > 0)
              fix_up_jbod(c)
              @controllers << c
            end
        end
        @controllers
      end

      def parse_assoc_dev_info(cntrlr,diskList)
        assocDiskList = []
        cntrlrFqdd    = cntrlr.controller_id
        if (diskList and diskList.length > 0)
          diskList.each do |enumDisk|
            tempArr = nil
            if (enumDisk.disk_id)
              tempArr = enumDisk.disk_id.split(":")
            else
              tempArr = enumDisk.vol_id.split(":")
            end
            if (cntrlrFqdd == tempArr.last)
              enumDisk.controller = cntrlr
              assocDiskList << enumDisk
            end
          end
        end
        assocDiskList
      end
      
      def get_controllers
        log("get all controllers.")
        puts "Enumerating all controllers on system"
        url = "#{WSMAN_URI_NS}/DCIM_ControllerView"
        controllerList = nil
        begin
          if (@wsman)
            xml = @wsman.command(ENUMERATE_CMD, url, " -m 512 -t 60000")
            if (!xml)
              puts "Unable to retrieve controller enumeration..Returning NIL"
              return nil
            end
            controllerList = @xml.processResponse(xml, '["Body"]["EnumerateResponse"]["Items"]["DCIM_ControllerView"]')
            controllerList = (controllerList.instance_of?(Array))?controllerList:[controllerList] if (controllerList)
            return controllerList
          else
            puts "Wsman object is null....not enumerating controllers"
            return controllerList
          end
        rescue Exception => e
          puts "Unable to retrieve controller id via wsman...Exception: #{e.message}"
          return nil
        end
      end

      def physical_disks
        log("physical_disks.")
        url = "#{WSMAN_URI_NS}/DCIM_PhysicalDiskView"
        xml = @wsman.command(ENUMERATE_CMD, url, " -m 512 -t 60000") if (@wsman)
        if (xml)
          phys_disks = @xml.processResponse(xml,'["Body"]["EnumerateResponse"]["Items"]["DCIM_PhysicalDiskView"]')
          return (phys_disks.nil?)?nil:parse_dev_info(phys_disks)
        else
          log("Unable to enumerate physical disks...returning nil")
          return nil
        end
      end
      
      def parse_dev_info(phys_disks)
        phys_disks = (phys_disks.instance_of?(Array))?phys_disks:[phys_disks]
        devs = []
        
        unless phys_disks.nil? 
          phys_disks.each do |disk| 
            rd = Crowbar::RAID::RaidDisk.new
            rd.disk_id         = disk['FQDD']
            rd.enclosure       = 0 # need to look into this,  WSMAN doesn't treat enclosures as integer values but FQDDs
            rd.slot            = disk['Slot'].to_i      
            rd.size            = disk['SizeInBytes'].to_i  
            rd.sas_address     = disk['SASAddress'].strip if disk['SASAddress'] and !disk['SASAddress'].is_a?(Hash)
            rd.serial_number   = disk['SerialNumber'].strip if disk['SerialNumber'] and !disk['SerialNumber'].is_a?(Hash)
            rd.model           = disk['Model'].strip if disk['Model']
            rd.manufacturer    = disk['Manufacturer'] if disk['Manufacturer']
            rd.protocol        = BUS_PROTO_MAP[disk['BusProtocol']] if disk['BusProtocol']
            rd.status          = CTRL_STATUS_MAP[disk['RollupStatus']] if disk['RollupStatus'] 
            rd.media_type      = MEDIA_TYPE_MAP[disk['MediaType']] if disk['MediaType']
            rd.raid_status     = disk['RaidStatus'] if disk['RaidStatus']
            if ((rd.media_type) and (rd.protocol))
              rd.media_type = rd.protocol.to_s + "_" + rd.media_type.to_s
            end
            devs << rd
          end
        end  
        devs
      end
      
      def boot_source_settings()
        log("boot_source_settings.")
        bss = nil
        url = "#{WSMAN_URI_NS}/DCIM_BootSourceSetting"
        xml = @wsman.command(ENUMERATE_CMD, url, "-m 512")
        if (xml)
          bss = @xml.processResponse(xml,'["Body"]["EnumerateResponse"]["Items"]["DCIM_BootSourceSetting"]')   
        else 
          puts "No boot source settings found...Returning nil"
        end
        bss
      end
      
      def boot_config_settings()
        log("boot_config_settings.")
        url = "#{WSMAN_URI_NS}/DCIM_BootConfigSetting"
        xml = @wsman.command(ENUMERATE_CMD, url, "-m 512")
        bcs = @xml.processResponse(xml,'["Body"]["EnumerateResponse"]["Items"]["DCIM_BootConfigSetting"]')   
        bcs
      end

      def raid_pd_states()
        log("raid_pd_states.")
        url = "#{WSMAN_URI_NS}/DCIM_RAIDEnumeration"
        xml = @wsman.command(ENUMERATE_CMD, url, "-m 512")
        re = @xml.processResponse(xml,'["Body"]["EnumerateResponse"]["Items"]["DCIM_RAIDEnumeration"]')  
        raidPds = Array.new 
        re.each do |re| 
         raidPds << re if re['AttributeName']=='RAIDPDState'
        end
        raidPds
      end
      
        
      def virtual_disks(physDiskList)
        log("virtual_disks.")
        url = "#{WSMAN_URI_NS}/DCIM_VirtualDiskView"
        xml = @wsman.command(ENUMERATE_CMD, url, "-m 512 -t 60000") if (@wsman)
        if (xml)
          virt_disks = @xml.processResponse(xml,'["Body"]["EnumerateResponse"]["Items"]["DCIM_VirtualDiskView"]')   
          return (virt_disks.nil?)?nil:parse_volumes(virt_disks,physDiskList)
        else
          log("Unable to enumerate virtual disks...returning nil")
          return nil
        end
      end
      
    def parse_volumes(virt_disks, physDiskList)
      log("parse_volumes.")
      virt_disks = (virt_disks.instance_of?(Array))?virt_disks:[virt_disks]
      vols = []    
      
      unless virt_disks.nil? 
        virt_disks.each do |vdisk| 
          rv = Crowbar::RAID::Volume. new()
          rv.vol_id      = vdisk["FQDD"]
          rv.vol_name    = vdisk["Name"] if vdisk["Name"] and !vdisk["Name"].is_a?(Hash)
          rv.size        = vdisk["SizeInBytes"]
          rv.span_depth  = vdisk["SpanDepth"]
          rv.span_length = vdisk["SpanLength"]
          rv.stripe_size = Integer(vdisk["StripeSize"])
          rv.members     = disks_by_id(vdisk["PhysicalDiskIDs"], physDiskList)
          rv.raid_level  = RAID_LEVEL_MAP[vdisk['RAIDTypes']] if vdisk['RAIDTypes']
          vols << rv
        end
      end  
      vols
    end
    
    def disks_by_id(disk_ids, disks)
      disk_ids = (disk_ids.instance_of?(Array))?disk_ids:[disk_ids]
      answer = []
      unless disk_ids.nil? || disks.nil?
        disk_ids.each do |disk_id|
          answer << disk_by_id(disk_id, disks)
        end
      end
      answer
    end
    
    def disk_by_id(disk_id, disks)
      disk = nil
      unless disks.nil?
        disks.each do |pd|
          next unless pd
          if disk_id == pd.disk_id
            disk = pd 
          end
        end
      end
      disk
    end
      
      def available_disks()
        log("available_disks.") 
        sub_cmd = "GetAvailableDisks"
        inputFile = "/tmp/#{sub_cmd}.xml"
        File.open("#{inputFile}", "w+") do |f|
          f.write %Q[
          <p:GetAvailableDisks_INPUT xmlns:p="http://schemas.dmtf.org/wbem/wscim/1/cim-schema/2/root/dcim/DCIM_RAIDService">
              <p:Target>#{@fqdd}</p:Target>
              <p:DiskType>0</p:DiskType>
              <p:Diskprotocol>0</p:Diskprotocol>
              <p:DiskEncrypt>0</p:DiskEncrypt>
           </p:GetAvailableDisks_INPUT>
          ]
        end
        
        cmd  = "#{INVOKE_CMD} -a #{sub_cmd}"
        if (@raid_svc_uri.nil?)
          @raid_svc_uri = @wsman.find_instance_uri(RAID_SVC_CLASS)
        end
        output = @wsman.command(cmd, @raid_svc_uri, "-J #{inputFile}")
        returnVal = @xml.returnValue(output, sub_cmd)
        if returnVal.to_i == RETURN_CFG_OK
          pd_array = @xml.processResponse(output,'["Body"]["GetAvailableDisks_OUTPUT"]["PDArray"]')
          return (pd_array.nil?)?nil:disks_by_id(pd_array)
        else
          failReason   = @xml.processResponse(output,'["Body"]["GetAvailableDisks_OUTPUT"]["Message"]')
          failReasonId = @xml.processResponse(output,'["Body"]["GetAvailableDisks_OUTPUT"]["MessageID"]')
          puts "GetAvailableDisks failed: #{failReasonId}:#{failReason}"
          return nil
        end
      end
      
      def check_vd_values(fqdd, availableDisks, raidLevel="2048")
        log "check_vd_values for: #{fqdd}."
        
        # generate the PDArray
        pdArray=""
        availableDisks.each do |diskFqdd|
          pdArray += "<p:PDArray>#{diskFqdd}</p:PDArray>"
        end
        
        sub_cmd = "CheckVDValues"
        inputFile = "/tmp/#{sub_cmd}.xml"
        File.open("#{inputFile}", "w+") do |f|
          f.write %Q[
          <p:CheckVDValues_INPUT xmlns:p="http://schemas.dmtf.org/wbem/wscim/1/cim-schema/2/root/dcim/DCIM_RAIDService">
            <p:Target>#{fqdd}</p:Target>
            #{pdArray}
            <p:VDPropNameArrayIn>RAIDLevel</p:VDPropNameArrayIn>
            <p:VDPropValueArrayIn>#{raidLevel}</p:VDPropValueArrayIn>
          </p:CheckVDValues_INPUT>
          ]
        end
        
        cmd  = "#{INVOKE_CMD} -a #{sub_cmd}"
        if (@raid_svc_uri.nil?)
          @raid_svc_uri = @wsman.find_instance_uri(RAID_SVC_CLASS)
        end
        output = @wsman.command(cmd, @raid_svc_uri, "-J #{inputFile}")
        returnVal = @xml.returnValue(output, sub_cmd)
        if returnVal.to_i == RETURN_CFG_OK
          keys = @xml.processResponse(output,'["Body"]["CheckVDValues_OUTPUT"]["VDPropNameArray"]')
          vals = @xml.processResponse(output,'["Body"]["CheckVDValues_OUTPUT"]["VDPropValueArray"]')
          return Hash[keys.zip(vals)]
        else
          failReason   = @xml.processResponse(output,'["Body"]["CheckVDValues_OUTPUT"]["Message"]')
          failReasonId = @xml.processResponse(output,'["Body"]["CheckVDValues_OUTPUT"]["MessageID"]')
          puts "CheckVDValues failed: #{failReasonId}:#{failReason}"
          return nil
        end
      end
      
      
      
      def describe
        "WSMAN RAID driver"
      end
      
     
      ## Raid config jobs now do not reboot the system immediately
      ## All raid config jobs across controllers are stacked in the 
      ## job queue and triggered via a reboot job 
      def create_raid_config_job(fqdd)
        log("create_raid_config_job.")
        wait_until_lc_ready()
        sub_cmd = "CreateTargetedConfigJob"
        inputFile = "/tmp/#{sub_cmd}_RAID.xml"
        File.open("#{inputFile}", "w+") do |ff|
          ff.write %Q[
            <p:CreateTargetedConfigJob_INPUT xmlns:p="http://schemas.dmtf.org/wbem/wscim/1/cim-schema/2/root/dcim/DCIM_RAIDService">
             <p:Target>#{fqdd}</p:Target>
             <p:ScheduledStartTime>TIME_NOW</p:ScheduledStartTime>
            </p:CreateTargetedConfigJob_INPUT>
          ] 
        end
        
        cmd  = "#{INVOKE_CMD} -a #{sub_cmd}"
        if (@raid_svc_uri.nil?)
          @raid_svc_uri = @wsman.find_instance_uri(RAID_SVC_CLASS)
        end
        xml = @wsman.command(cmd, @raid_svc_uri, "-J #{inputFile}")
        returnVal = @xml.returnValue(xml,sub_cmd)
        if returnVal.to_i == RETURN_CONFIG_VAL_OK
          response = @xml.processResponse(xml,'["Body"]["CreateTargetedConfigJob_OUTPUT"]')
          jobData = @wsman.get_job_id(response["Job"])
          [RETURN_CONFIG_VAL_OK, jobData]
        else
          response = @xml.processResponse(xml,'["Body"]["CreateTargetedConfigJob_OUTPUT"]')
          raise "Could not create raid config job, response: #{response}"
          [RETURN_VAL_FAIL, nil]
        end
      end

      ## Utility method to set up multiple RAID jobs in the job queue
      def run_multiple_raid_jobs(jobArray)
        retValue = true
        puts "Rebooting system to run RAID configuration jobs"
        %x[reboot && sleep 120]
        retValue

        ## RKR: Need to scrub...throws errors for scheduling job
        ## if reboot job is used in the queue...
        #if (jobArray and jobArray.length > 0)
        #  retVal,jobID = @wsman.create_reboot_job()
        #  if (retVal)
        #    jobArray.unshift(jobID)
        #    retVal = @wsman.setup_job_queue_multi(jobArray)
        #    if (retVal)
        #      retValue = true 
        #    else
        #      puts "Unable to set up the job queue for RAID jobs"
        #    end
        #  else
        #    puts "Unable to create a reboot job...Exiting"
        #  end
        #end
        retValue 
      end


      def create_bios_config_job()
        log("create_bios_config_job.")
        wait_until_lc_ready()
        sub_cmd = "CreateTargetedConfigJob"
        inputFile = "/tmp/#{sub_cmd}_BIOS.xml"
        File.open("#{inputFile}", "w+") do |ff|
          ff.write %Q[
               <p:CreateTargetedConfigJob_INPUT xmlns:p="http://schemas.dmtf.org/wbem/wscim/1/cim-schema/2/root/dcim/DCIM_BIOSService">
                 <p:Target>BIOS.Setup.1-1</p:Target>
                 <p:RebootJobType>1</p:RebootJobType>
                 <p:ScheduledStartTime>TIME_NOW</p:ScheduledStartTime>
               </p:CreateTargetedConfigJob_INPUT>
          ] 
        end
        
        cmd  = "#{INVOKE_CMD} -a #{sub_cmd}"
        bios_svc_uri = @wsman.find_instance_uri(BIOS_SVC_CLASS)
        xml = @wsman.command(cmd, bios_svc_uri, "-J #{inputFile}")
        returnVal = @xml.returnValue(xml,sub_cmd)
        
        if returnVal == RETURN_CONFIG_VAL_OK
          return RETURN_CONFIG_VAL_OK
        else
          response = @xml.processResponse(xml,'["Body"]["CreateTargetedConfigJob_OUTPUT"]')
          raise "Could not create bios config job, response: #{response.inspect}"
          return RETURN_VAL_FAIL
          
        end
      end
     

      ## Utility method to delete the pending configuration on all 
      ## enumerated controllers...
      def delete_pending_config_on_controllers(controllerList)
        if (controllerList and controllerList.length > 0)
          controllerList.each do |cntrlr|
            delete_pending_config(cntrlr['FQDD'])
          end
        end
      end

      ## Deletes any pending configuration changes on the controller fqdd
      ## so that no configuration changes are actually committed
      def delete_pending_config(fqdd)
        log("delete_pending_config.")
        puts "Deleting pending configuration on #{fqdd}"
        wait_until_lc_ready()
        sub_cmd = "DeletePendingConfiguration"
        returnVal = RETURN_VAL_FAIL
        inputFile = "/tmp/#{sub_cmd}_RAID.xml"

        if (!@wsman)
          puts "DBG: WSman object is null...returning from delete of pending config"
        end

        File.open("#{inputFile}", "w+") do |ff|
          ff.write %Q[
            <p:DeletePendingConfiguration_INPUT xmlns:p="http://schemas.dmtf.org/wbem/wscim/1/cim-schema/2/root/dcim/DCIM_RAIDService">
                <p:Target>#{fqdd}</p:Target>
             </p:DeletePendingConfiguration_INPUT>
          ] 
        end
        
        cmd  = "#{INVOKE_CMD} -a #{sub_cmd}"
        if (@raid_svc_uri.nil?)
          @raid_svc_uri = @wsman.find_instance_uri(RAID_SVC_CLASS) 
        end
        if (@wsman)
          xml = @wsman.command(cmd, @raid_svc_uri, "-J #{inputFile}")
          returnVal = @xml.returnValue(xml,sub_cmd) if (xml)
        else
          puts "WSMan object is null..not deleting pending config"
        end
        return returnVal
    end
    
=begin
       Ideally I would have liked to call check_vd_values and have it return the params I should use but 
        it did not work out, many attribute errors with no trace from WSMAN.
        #generate properties array code
         propArray = ""
        vd_values.each do |key, value|
          propArray +="<p:VDPropNameArrayIn>#{key}</p:VDPropNameArrayIn>\n<p:VDPropValueArrayIn>#{value}</p:VDPropValueArrayIn>"
        end 
=end      

      def evaluate_array_size(disk_size_arr, raid_level)
        ret_val       = 0
        max_vol_size  = (Crowbar::RAID::TERA * 2 - Crowbar::RAID::MEGA) 
        case raid_level
          when :RAID0
          ## size of volume is numDisks * size of lowest disk
          ret_val = disk_size_arr[0] * disk_size_arr.length

          when :RAID1
          ## size of volume is just size of the lowest disk
          ret_val = disk_size_arr[0] 

          when :RAID10
          ## size of volume is (numDisks / 2) * size of lowest disk
          ret_val = (disk_size_arr.length / 2) * disk_size_arr[0]

          when :JBOD
          #pseudo raid0 level
          ret_val = disk_size_arr[0]
        end 
        ret_val = max_vol_size if (ret_val > max_vol_size)
        puts "DBG: returning size of RAID volume to be #{ret_val}"
        return ret_val
      end

      def create_vd(cntrlr,volume_description)
        log("create_vd, raid_level: #{volume_description[:type]}")
        convert_to_raid(volume_description[:disks]) 
        sub_cmd = "CreateVirtualDisk" 
        inputFile = "/tmp/#{sub_cmd}.xml"
        max_size = nil
        cmd      = "#{INVOKE_CMD} -a #{sub_cmd}"
        disks    = volume_description[:disks]
        name     = volume_description[:name] if volume_description[:name]
        max_size = volume_description[:size] if volume_description[:size] != "MAX"
        stripe_size = volume_description[:stripe_size]
        max_vol_size       = (Crowbar::RAID::TERA * 2 - Crowbar::RAID::MEGA) 
        disk_size_arr      = []

       
        spanLength = disks.length 
        spanDepth = disks.length
        returnVal = nil;
        if (@raid_svc_uri.nil?)
          @raid_svc_uri = @wsman.find_instance_uri(RAID_SVC_CLASS)
        end

        begin
          raid_level = volume_description[:type]
          ## Limit VD size on platforms like R720 to less than 2 TB
          current_mode, pending_mode = @wsman.get_current_and_pending_bootmode()
          puts "DBG: Curr boot mode = #{current_mode}. Pending boot mode = #{pending_mode}"
          ## BIOS barclamp is done by the time RAID VDs are created ...
          if (current_mode and !current_mode.is_a?(Hash) and current_mode == "Bios")
            if (max_size and max_size > max_vol_size)
              max_size = max_vol_size
            else
              disks.each do |disk|
                disk_size_arr << disk.size 
              end
              disk_size_arr.sort!
              max_size = evaluate_array_size(disk_size_arr, raid_level)
            end
          end

          case raid_level
            when :RAID0
            create_volume_input_file(inputFile, name, disks, RAID0_VAL, 1, disks.length, max_size, stripe_size)
            xml = @wsman.command(cmd, @raid_svc_uri, "-J #{inputFile}")
            returnVal = @xml.returnValue(xml,sub_cmd)

            when :RAID1
            spanDepth = disks.length/SPAN_LENGTH
            create_volume_input_file(inputFile, name, disks, RAID1_VAL, spanDepth, SPAN_LENGTH, max_size, stripe_size)
            xml = @wsman.command(cmd, @raid_svc_uri, "-J #{inputFile}")
            returnVal = @xml.returnValue(xml,sub_cmd)

            when :RAID10
            disk_info = RAID10_SPAN_TABLE[disks.length]
            if disk_info
              spanLength = disk_info[:span_length]
              spanDepth = disk_info[:span_depth]
            else
              spanLength = SPAN_LENGTH
              spanDepth = disks.length/spanLength
            end
            # TODO: Handle the EEK case of 22 and 26.
            create_volume_input_file(inputFile, name, disks, RAID10_VAL, spanDepth, spanLength, max_size, stripe_size)
            xml = @wsman.command(cmd, @raid_svc_uri, "-J #{inputFile}")
            returnVal = @xml.returnValue(xml,sub_cmd)

            when :JBOD 
            disks.each do |disk|
              create_volume_input_file(inputFile, name, [disk], RAID0_VAL, 1, 1, max_size, stripe_size)
              xml = @wsman.command(cmd, @raid_svc_uri, "-J #{inputFile}")
              returnVal = @xml.returnValue(xml,sub_cmd)
              break unless returnVal == RETURN_VAL_OK
            end

            when :RAID5
            create_volume_input_file(inputFile, name, disks, RAID5_VAL, 1, disks.length, max_size, stripe_size)
            xml = @wsman.command(cmd, @raid_svc_uri, "-J #{inputFile}")
            returnVal = @xml.returnValue(xml,sub_cmd)

            when :RAID6
            create_volume_input_file(inputFile, name, disks, RAID6_VAL, 1, disks.length, max_size, stripe_size)
            xml = @wsman.command(cmd, @raid_svc_uri, "-J #{inputFile}")
            returnVal = @xml.returnValue(xml,sub_cmd)
            
          else
            raise "unknown raid level requested: #{raid_level}" 
          end
          
          if returnVal==RETURN_VAL_OK
            RETURN_VAL_OK
          else #return the error
            raise "failed create volume, response: #{@xml.processResponse(xml,'["Body"]["CreateVirtualDisk_OUTPUT"]')}" 
          end
        rescue Exception => e
          log("Create volume failed, reason: #{e.message}", :ERROR)
          delete_pending_config(cntrlr.controller_id)
          RETURN_VAL_FAIL
        end 
      end

      def convert_to_raid(disks)
        log("convert_to_raid.")  
        toRaid = Array.new

        disks.each do |disk|
          if disk.raid_status and disk.raid_status.to_i == 8
            toRaid << disk
          end
        end
        return RETURN_VAL_NO_ACTION unless toRaid.size != 0
        begin
          cmd  = "#{INVOKE_CMD} -a #{CONVERT_TO_RAID_CMD}"
          create_ctr_input_file(toRaid)
          if (@raid_svc_uri.nil?)
            @raid_svc_uri = @wsman.find_instance_uri(RAID_SVC_CLASS)
          end
          xml = @wsman.command(cmd, @raid_svc_uri, "-J /tmp/#{CONVERT_TO_RAID_CMD}.xml")
          log(xml.inspect)
          returnVal = @xml.returnValue(xml,CONVERT_TO_RAID_CMD)
          ## RKR: No need to reboot system with a job for converting to RAID...
          ## should be in pending state and allow all other operations to go through
          #return create_raid_config_job() unless returnVal != RETURN_VAL_OK
          returnVal
        rescue Exception => e
          log("convert to raid failed, reason: #{e.message}", :ERROR)
          delete_pending_config()
          RETURN_VAL_FAIL
        end
      end
      
      def create_ctr_input_file(disks)
        pdArray=""
        disks.each do |disk|
          pdArray += "<p:PDArray>#{disk.disk_id}</p:PDArray>"
        end

        File.open("/tmp/#{CONVERT_TO_RAID_CMD}.xml", "w+") do |f|
          f.write %Q[
          <p:ConvertToRAID_INPUT xmlns:p="http://schemas.dmtf.org/wbem/wscim/1/cim-schema/2/root/dcim/DCIM_RAIDService">
            #{pdArray}
          </p:ConvertToRAID_INPUT>
          ]
        end
      end
      
      
      def create_volume_input_file(path, name, disks, raid_level, span_depth, span_length, max_size, stripe_size = nil)
        pdArray="" 
        disks.each do |disk|
          pdArray += "<p:PDArray>#{disk.disk_id}</p:PDArray>" 
        end
        fqdd = disks[0].controller.controller_id
        
        sizeProp = ""
        sizeVal = ""
        isJBOD = (raid_level==RAID0_VAL && span_depth==1 && span_length==1)
        if (not isJBOD and max_size)
          sizeProp = "<p:VDPropNameArray>Size</p:VDPropNameArray>"
          sizeVal = "<p:VDPropValueArray>#{max_size/MEGA}</p:VDPropValueArray>"
        end

        stripeSizeProp = ""
        stripeSizeVal = ""
        if stripe_size
          stripeSizeProp = "<p:VDPropNameArray>StripeSize</p:VDPropNameArray>"
          stripeSizeVal = "<p:VDPropValueArray>#{stripe_size * 2}</p:VDPropValueArray>" # VALUE IN KB * 2 matches enum
        end

            #<p:VDPropValueArray>#{name}</p:VDPropValueArray>
            #<p:VDPropNameArray>VirtualDiskName</p:VDPropNameArray>
        File.open("#{path}", "w+") do |f|
          f.write %Q[
          <p:CreateVirtualDisk_INPUT xmlns:p="http://schemas.dmtf.org/wbem/wscim/1/cim-schema/2/root/dcim/DCIM_RAIDService">
            <p:Target>#{fqdd}</p:Target>
            #{pdArray}
            <p:VDPropNameArray>RAIDLevel</p:VDPropNameArray>
            <p:VDPropNameArray>SpanDepth</p:VDPropNameArray> 
            <p:VDPropNameArray>SpanLength</p:VDPropNameArray>
            #{sizeProp}
            #{stripeSizeProp}
            <p:VDPropValueArray>#{raid_level}</p:VDPropValueArray>
            <p:VDPropValueArray>#{span_depth}</p:VDPropValueArray>
            <p:VDPropValueArray>#{span_length}</p:VDPropValueArray>
            #{sizeVal}
            #{stripeSizeVal}
          </p:CreateVirtualDisk_INPUT>
          ]
          end
      end
      
      ## RKR: With new changes to reset the configuration this code for ##
      ## deleting individual and all volumes should never get hit again ##
      def delete_vd_by_id(cid, vid)
        log("delete_vd.")
        begin
          volume_id  = vid
          sub_cmd = "DeleteVirtualDisk"
          inputFile = "/tmp/#{sub_cmd}.xml"
          File.open("#{inputFile}", "w+") do |f|
            f.write %Q[
          <p:#{sub_cmd}_INPUT xmlns:p="http://schemas.dmtf.org/wbem/wscim/1/cim-schema/2/root/dcim/DCIM_RAIDService">
            <p:Target>#{volume_id}</p:Target>
          </p:#{sub_cmd}_INPUT>
          ]
          end
          
          cmd  = "#{INVOKE_CMD} -a #{sub_cmd}"
          if (@raid_svc_uri.nil?)
            @raid_svc_uri = @wsman.find_instance_uri(RAID_SVC_CLASS)
          end
          xml = @wsman.command(cmd, @raid_svc_uri, "-J #{inputFile}")
          returnVal = @xml.returnValue(xml,sub_cmd)
          raise "DeleteVirtualDisk call failed with return code: #{returnVal}" if returnVal.nil? || returnVal != RETURN_VAL_OK
          RETURN_VAL_OK
        rescue Exception => e 
          log("Failed deleting volume, exception: #{e.message}", :ERROR)
          delete_pending_config(cid)
          RETURN_VAL_FAIL # failure code.
        end
      end

      def delete_vd(volume)
        if volume.raid_level == :JBOD
          fail = false
          volume.members.each do |d|
            next unless d.vol_id
            answer = delete_vd_by_id(d.controller.controller_id, d.vol_id)
            fail = true if answer == RETURN_VAL_FAIL 
          end
          RETURN_VAL_FAIL if fail
          RETURN_VAL_OK unless fail
        else
          delete_vd_by_id(volume.controller.controller_id, volume.vol_id)
        end
      end
     
      ## Currently the set_boot does not set HDD first...always sets NIC ##
      ## first...In theory when the state transitions out of installed   ##
      ## we should boot off HDD and not serve up a file during PXE boot  ##                        
      def set_boot(controller, volume, nic_first = true)
        log("Setting boot sources...set_boot recipe")
        return_val   = RETURN_VAL_NO_ACTION
        need_cfg_job = false
        bios_fqdd    = "BIOS.Setup.1-1"

        begin
          retStatus , need_cfg_job = @wsman.check_and_handle_boot_sources()
          if (need_cfg_job)
            bios_svc_uri = @wsman.find_instance_uri(BIOS_SVC_CLASS)
            @wsman.create_targeted_config_job(bios_svc_uri, bios_fqdd)
            return_val = RETURN_VAL_OK
          else
            puts "DBG: No targeted config job created...CfgJob boolean is false"
          end
          if (!retStatus)
            ## Either enabling boot sources or re-ordering them failed
            ## should not be an issue because worst case it's still going
            ## to directly fall to the HDD and not pxe boot and fail
            puts "DBG: Issue re-ordering NICs...not deleting controller config"
          end
        rescue Exception => e
          log("Failed setting boot, exception: #{e.message}", :ERROR)
          delete_pending_config(controller.controller_id)
          return_val = RETURN_VAL_FAIL
        end
        return return_val
      end

      def config_csior()
        if !is_csior_enabled()
          returnVal = enable_csior()
          if returnVal == RETURN_VAL_OK
             create_bios_config_job()
             sleep()
          end
        end 
      end
      
      
      def is_csior_enabled()
        log "check_csior_enabled."
        xml = @wsman.command(GET_CMD, CSIOR_ATTR_URI)
        returnVal = @xml.processResponse(xml,'["Body"]["DCIM_LCEnumeration"]["CurrentValue"]')
        return !returnVal.nil? && returnVal=="Enabled"
      end
      
      def enable_csior()
        log "enable_csior."
        wait_until_lc_ready()
        sub_cmd = "SetAttribute"
        inputFile = "/tmp/#{sub_cmd}_LC.xml"
        File.open("#{inputFile}", "w+") do |ff|
          ff.write %Q[
            <p:SetAttribute_INPUT xmlns:p="http://schemas.dmtf.org/wbem/wscim/1/cim-schema/2/root/dcim/DCIM_LCService">
              <p:AttributeName>#{CSIOR_ATTR}</p:AttributeName>
              <p:AttributeValue>Disabled</p:AttributeValue>
            </p:SetAttribute_INPUT>
          ] 
        end
        
        cmd  = "#{INVOKE_CMD} -a #{sub_cmd}"
        lc_svc_uri = @wsman.find_instance_uri(LC_SVC_CLASS)
        xml = @wsman.command(cmd, lc_svc_uri, "-J #{inputFile}")
        #check returnValue
        returnVal = @xml.returnValue(xml,sub_cmd)
        log "enable_csior return val is: #{returnVal}"
        return returnVal
        
      end

      def cleanup_test()
        log "cleanup."
        load_info()
        if (!@volumes.nil? && @volumes.length !=0)
          returnVal = delete_all_volumes()
          if(RETURN_VAL_OK==returnVal)
            job_id = create_raid_config_job()
            if job_id[0,4]!="JID_"
              log "Error creating config job: #{job_id}"
              exit
            else
              percent_complete = ""
              until percent_complete == "100"
                percent_complete = job_status(job_id)
                log "delete volumes job status percent complete: #{percent_complete}" 
                sleep 5;
              end
              log "Delete all volumes done!!!!"
            end 
          end
        end
      end
      
      def create_vd_test(raid_level,name)
        log "create_vd_test."
        log "Available Disks: #{@available.inspect}"
        if (@available.nil? || @available.length == 0)
          log "Unabled to find available disks for creating volume" 
          exit
        end
        
        returnVal = create_volume(raid_level, name, @available, 100)
        log "after create volume returnVal is: #{returnVal}" 
        if returnVal !=RETURN_VAL_OK #create VD call not successful
          log "Failed creating volume, wsman returned: #{returnVal.inspect}, deleting pending config"
          delete_pending_config()
        else
          job_id = create_raid_config_job()
          if job_id[0,4]!="JID_"
            log "Error creating config job: #{job_id}"
            exit
          else
            percent_complete = ""
            until percent_complete == "100"
              percent_complete = job_status(job_id)
              log "job status percent complete: #{percent_complete}" 
              sleep 1;
            end
            log "Create Volume Done!!!!"
          end
        end
      end


      def clear_controller_config(cntrlr, cmd)
        fqdd = cntrlr.controller_id
        sub_cmd = RESET_CONF_CMD
        sub_cmd = CLEAR_FOREIGN_CMD if (cmd == :foreign)
        cmdStr = "#{INVOKE_CMD} -a #{sub_cmd} -k Target=#{fqdd}"
        log "Clearing controller config (#{sub_cmd}) for #{fqdd}"
        if (@raid_svc_uri.nil?)
          @raid_svc_uri = @wsman.find_instance_uri(RAID_SVC_CLASS)
        end
        xml = @wsman.command(cmdStr, @raid_svc_uri)
        returnVal = @xml.returnValue(xml,sub_cmd)
        if returnVal.to_i == RETURN_CFG_OK
          log "Successfully cleared controller config"
          return true
        else
          return false
        end
      end

      ## Utility method to return all possible RAID levels for ##
      ## the given controller                                  ##
      def get_all_possible_raid_levels(cntrlr)
        log("Determining all possible RAID levels for #[cntrlr.controller_id}")
        url = "#{WSMAN_URI_NS}/DCIM_RAIDEnumeration"
        begin
          if (@wsman)
            output = @wsman.command(ENUMERATE_CMD, url, " -m 512 --dialect \"http://schemas.dmtf.org/wbem/cql/1/dsp0202.pdf\" --filter \"select * from DCIM_RAIDEnumeration where AttributeName='RAIDSupportedRAIDLevels' AND FQDD='#{cntrlr.controller_id}'\" ")
            if (!output)
              puts "Unable to retrieve RAID attributes"
            else
              raid_enum = @xml.processResponse(output, '["Body"]["EnumerateResponse"]["Items"]["DCIM_RAIDEnumeration"]["PossibleValues"]')
              if (raid_enum)
                cntrlr.raid_capable = true
                raid_enum = (raid_enum.instance_of?(Array))?raid_enum:[raid_enum]
                raid_enum.each do |raidLevel|
                  cntrlr.supported_raid_levels << RAID_ENUM_MAP[raidLevel]
                  cntrlr.supported_raid_levels << :JBOD if RAID_ENUM_MAP[raidLevel] == :RAID0
                end
              end 
            end
          end
        rescue Exception => e
          log("Failed enumerating RAID attributes,exception: #{e.message}", :ERROR)
        end
      end

    end 
  end 
end

if __FILE__ == $0
  require 'wsman' 
  require 'xml_util'
  host = '192.168.124.32'
  user = 'root'
  password = 'cr0wBar!'
  port = 443
  node = Hash.new
  node[:ipmi] = Hash.new
  node[:ipmi][:bmc_user] = user
  node[:ipmi][:bmc_password] = password
  node["crowbar_wall"] = Hash.new
  node["crowbar_wall"]["ipmi"] = Hash.new
  node["crowbar_wall"]["ipmi"]["address"] = host
  puts "RKR:#{node.inspect}"
  
  $in_chef = false
  puts '....................... wsman_cli tester.......................'
  opts = { :user => user, :password => password, :host => host, :port => 443, :debug_time => true }
  wsman = Crowbar::WSMAN.new(node)
  raid = Crowbar::RAID::WsManCli.new(wsman)
  c = raid.enumerate_topology
  c.each do |con|
    puts "controller:Disks(#{con.disks.length}) Volumes(#{con.volumes.length})"
    con.disks.each {|d| puts "Disk #{d.disk_id}:#{d.slot} = #{d.size}" }
    con.volumes.each {|v| puts "Volume #{v.vol_id}:#{v.raid_level} = #{v.size}" }
  end
  
  #fqdd = raid.find_controller()
  #raid.log "Found raid controller #{fqdd}"
  
  # raid.delete_pending_config()
  # exit
  
  # raid.load_info()
  
  # raid.available_disks(raid.fqdd)
  # raidLevels = raid.load_raid_levels(fqdd,availableDisks)
  # raid.log "RAID Levels: #{raidLevels.inspect}"
  # vd_values = raid.check_vd_values(fqdd,availableDisks,"2048")
  # raid.log "check_vd_values ret: #{vd_values.inspect}"
  #raid.config_csior()
 #  raid.create_vd_test(:RAID10,"test_volumexxx")

  ## clean up
  # sleep(300)
  # raid.cleanup_test()
end
