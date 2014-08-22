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

# Sample config block: can be found in the data_bags/crowbar-data dir

#
# Actions from resource
#
action :report do  
  # Implicitly handled by load_current_resource
end

action :apply do
  config = @new_resource.config
  apply_config(config, @controllers) unless @failed
end

action :set_boot do
  config = @new_resource.config
  nic_first = @new_resource.nic_first
  do_set_boot(config, @controllers, nic_first) unless @failed
end

#### Chef override functions
#
# Called on each action call on this resource.
#
# Sets @failed to help actions return faster.
#
def load_current_resource
  log "load_current_resource start..............................."
  @drivers, @controllers = enumerate_topology
  if @controllers.nil? or 
     @drivers.nil? or 
     @controllers.empty? or 
     @drivers.empty?
    @failed = true
  else
    do_report(@new_resource.debug_flag)
  end
  log "load_current_resource end............................"
end

### Helper functions ####
#
# log to the problem file.
#
def report_problem(msg)
  problem_file = @new_resource.problem_file
  log_("reporting problem to: #{problem_file}- #{msg}" )
  unless problem_file.nil?
    open(problem_file,"a") { |f| f.puts(msg) }
  end
  log_(msg)
end

def log_(msg)
  Chef::Log.info(msg) 
  true
end


def log_action(action)
  node["crowbar_wall"] = {} unless node["crowbar_wall"]
  node["crowbar_wall"]["raid"] = {} unless node["crowbar_wall"]["raid"]
  node["crowbar_wall"]["raid"]["actions"] = [] unless node["crowbar_wall"]["raid"]["actions"]
  node["crowbar_wall"]["raid"]["actions"] << action
end

#
# Function to gather a report
#
# This is called from load_current_resource only on success
#
def do_report(debug_flag)
  s = "\n"
  s << "Current RAID configuration report:\n"

  node["crowbar_wall"] ||= {}
  node["crowbar_wall"]["raid"] ||= {}
  node["crowbar_wall"]["raid"]["drivers"] = []
  node["crowbar_wall"]["raid"]["controllers"] = {}

  s << " drivers:\n"
  @drivers.each do |d|
    d.debug = debug_flag
    node["crowbar_wall"]["raid"]["drivers"] << d.describe
    s << "  #{d.describe}\n"
  end

  s << " controllers: #{@controllers.length}\n"
  @controllers.each do |c|
    node["crowbar_wall"]["raid"]["controllers"][c.controller_id] = c.to_hash
    s << "  Controller #{c.controller_id}:\n"
    s << "   disks #{c.disks.length}: #{c.disks.map {|x| x.to_s}.join(", ")}\n"
    s << "   volumes #{c.volumes.length}: #{c.volumes.map {|x| x.to_s}.join(", ")}\n"
  end
  
  log s

  node.save    
rescue Exception => e
  log("do_report exception: #{e.message} #{e.backtrace}")
  report_problem($!)  ## $! is the global exception variable
end

#
# Walks the set of drivers and returns if the a set returns controllers
#
# Return a [ drivers, controllers ]
#
def enumerate_topology
  driver_sets = [[Crowbar::RAID::WsManCli],[Crowbar::RAID::LSI_MegaCli,Crowbar::RAID::LSI_sasIrcu]]  
  drivers = []
  controllers = []

  driver_sets.each do |set|
    log("will try set: #{set.inspect}")

    set.each do |c|
      log("trying #{c}") {level :debug}
      puts "Trying new driver #{c}"
      ## try to instantiate and test the controller class, but catch errors.
      begin
        driver = c.new(node)
        cntls = driver.enumerate_topology
        puts "#{c}.enumerate_topology returned #{cntls}"
        unless cntls.empty?
          drivers << driver
          controllers << cntls
        end
      rescue Exception => e
        log("failed to test #{c}, error: #{e.message} trace: #{e.backtrace}")
        puts "Exception caught in enumerate_topology in CHEF PROVIDER: #{e.message}"
      end
    end

    break unless controllers.empty?
  end
  controllers = controllers.flatten
  log("no suported RAID controller found on this system"){level :error} if controllers.empty?
  [drivers, controllers]
end

#
# raid info.
#
RAID_INFO = {
    :RAID0  => { :min_count => 2, :max_count => 10000, :odd => false, :even => false },
    :RAID00 => { :min_count => 4, :max_count => 10000, :odd => false, :even => true },
    :RAID1  => { :min_count => 2, :max_count => 2,     :odd => false, :even => true },
    :RAID10 => { :min_count => 4, :max_count => 10000, :odd => false, :even => true },
    :RAID1E => { :min_count => 3, :max_count => 10000, :odd => true,  :even => false },
    :RAID5  => { :min_count => 3, :max_count => 10000, :odd => false, :even => false },
    :RAID50 => { :min_count => 6, :max_count => 10000, :odd => false, :even => true },
    :RAID6  => { :min_count => 4, :max_count => 10000, :odd => false, :even => false },
    :RAID60 => { :min_count => 8, :max_count => 10000, :odd => false, :even => true },
    :JBOD   => { :min_count => 1, :max_count => 10000, :odd => false, :even => false }
}

def get_disk_count_min(rl)
  return RAID_INFO[rl][:min_count] if RAID_INFO[rl]
  1
end

def get_disk_count_max(rl)
  return RAID_INFO[rl][:max_count] if RAID_INFO[rl]
  10000
end

def should_be_even?(rl)
  return RAID_INFO[rl][:even] if RAID_INFO[rl]
  false
end

def should_be_odd?(rl)
  return RAID_INFO[rl][:odd] if RAID_INFO[rl]
  false
end

#
# Given a config and the enumerated data, build a set of new
# volumes independent of current volumes
#
# Return: [volumes, errors]
#   volumes = list of volume structures
#   errors = list of errors
#
def build_volumes(config, controllers)
  volumes = []
  errors = []

  return [ [], [] ] unless config["volumes"]

  # How many controllers are explicitly declared by volume
  cont_names = []
  config["volumes"].each do |k,v|
    cont_names << v["controller"] if v["controller"]
  end
  cont_names.uniq!

  # Process controllers to get option lists.  
  # Given the names, find the best matches.
  cont_options = {}
  cont_names.each do |c_name|
    c = config["controllers"][c_name] rescue nil
    if c
      cont_options[c_name] = []

      controllers.each do |rc|
        # Test for controller match (Check schema)

        # Product_name test
        if c["device"]
          next unless c["device"].include? rc.product_name
        end

        # Filter by supported raid levels
        if c["raid_level_filter"]
          list = c["raid_level_filter"].map{ |x| x.to_sym }
          new_list = rc.supported_raid_levels & list
          next if new_list.empty?
        end
 
        cont_options[c_name] << rc
      end
    else
      # No criteria, take them all
      cont_options[c_name] = controllers.dup
    end
  end 
  cont_options.each do |k,l|
    next if l.length > 0
    errors << "Controller #{k} has no options"
  end

  unused_controllers = controllers.dup
  # Set up available disk arrays
  controllers.each do |c|
    c.avail_disks = c.disks.dup
  end

  # Sort config volumes.
  config_vols = config["volumes"].sort { |a,b| a[1]["order"] <=> b[1]["order"] }

  # Fill as appropriate.
  config_vols.each do |item|
    name = item[0]
    hash = item[1]

    c_set = unused_controllers
    c_set = cont_options[hash["controller"]] if hash["controller"]
    
    if c_set.nil? or c_set.empty?
      errors << "Volume #{name} doesn't have viable controller options"
      next
    end

    rl = hash["raid_level"].to_sym
    disk_min = get_disk_count_min(rl)
    disk_max = get_disk_count_max(rl)

    # Validate that enough requested disks work for Raid_level
    disk_count = hash["disk_count"]
    if disk_count and disk_count != "remaining"
      disk_count = disk_count.to_i
      if disk_count < disk_min
        errors << "Volume #{name} is requesting less disks than required"
        next
      end
      disk_min = disk_count
      # Make sure we aren't over the max disk count
      if disk_count > disk_max
        disk_count = disk_max
      end
    end

    my_c = nil
    c_set.each do |c|
      next unless c.supported_raid_levels.include? rl
      if disk_min > c.avail_disks.length 
        # This is the hack to downgrade a RAID10 to a RAID1
        # for our C6100s
        if rl == :RAID10 and c.avail_disks.length > 1
          rl = :RAID1
          disk_min = 2
          disk_count = 2
        else
          next
        end
      end

      # GREG: Disk size checks.
      # GREG: other controller validation here.

      my_c = c
      break
    end

    unless my_c
      errors << "Volume #{name} doesn't match any controller"
      next
    end

    #
    # If a named controller, set it and don't let others use it.
    #
    if hash["controller"]
      unused_controllers.delete(my_c)
      cont_options.each do |k,h|
        h.delete(my_c)
      end
      cont_options[hash["controller"]] = [ my_c ]
    end

    #
    # Create volume on this thing!
    #
    v = Crowbar::RAID::Volume.new
    v.controller = my_c
    v.raid_level = rl
    v.vol_name = name
    if hash["max_size"] 
      v.size = hash["max_size"]
    else
      v.size = "MAX"
    end
    if hash["stripe_size"] 
      v.stripe_size = hash["stripe_size"]
    end
    
    disk_count = my_c.avail_disks.length if disk_count == "remaining"
    if should_be_even? rl and disk_count % 2 == 1
      disk_count = disk_count - 1
    end
    if should_be_odd? rl and disk_count % 2 == 0
      disk_count = disk_count - 1
    end
    if disk_count > get_disk_count_max(rl)
      disk_count = get_disk_count_max(rl)
    end
    if disk_count < get_disk_count_min(rl)
      errors << "Volume #{name} can't be made because remaining disk are too few"
      next
    end

    disk_count.times do
      v.members << my_c.avail_disks.shift
    end
    
    volumes << v
  end

  return [ [], errors ]  unless errors.empty?

  #
  # Once all volumes are constructed, build JBOD volumes for the remaining drives.
  # This is an implicit default.
  #
  disks = {}
  controllers.each { |c| disks[c] = c.disks.dup }
  volumes.each do |v|
    cc = v.controller
    disks[cc] = disks[cc] - v.members
  end
  
  disks.keys.each do |k|
    next if disks[k].length == 0
    rv = Crowbar::RAID::Volume.new
    rv.controller = k
    rv.raid_level = :JBOD
    rv.members = disks[k]
    volumes << rv
  end

  [ volumes, [] ]
end

#
# Compare to volumes on the same controller.
#
def compare_volume(old, new)
  return 0 if old.nil? and new.nil?
  return -1 if new.nil?
  return 1 if old.nil?

  reason = "no diff"
  begin 
    ret = 0
    if old.controller != new.controller
      reason = "different controller" ;ret=1 ; break  
    end
    if old.members.length != new.members.length
      reason = "different member's count #{old.members.length}/#{new.members.length}" ; ret=1 ; break 
    end
    if  old.raid_level != new.raid_level
      reason = "different level" ; ret= 1 ; break 
    end
  
    ostr = old.members.map { |d| "#{d.enclosure}:#{d.slot}" }.sort.join(",")
    nstr = new.members.map { |d| "#{d.enclosure}:#{d.slot}" }.sort.join(",")
    reason = "different members" && ret = ostr <=> nstr 
  end while false
  if ret != 0
    log("volume #{old.vol_name} differ because of #{reason}")
  else
    log("volume #{old.vol_name} exists")
  end
  ret
end

def get_volume_differences(new_volumes, controllers)
  old_volumes = controllers.map { |x| x.volumes }
  old_volumes.flatten!
  
  # 
  # Remove matching volumes
  #
  rm_old = []
  rm_new = []
  old_volumes.each do |ov|
    new_volumes.each do |nv|
      if compare_volume(ov, nv) == 0
        rm_old << ov
        rm_new << nv
        nv.vol_id = ov.vol_id
        log("reusing #{ov.vol_id} for #{nv.vol_name}/#{nv.vol_id}")
      else
        log("new volume #{nv.vol_name}/#{nv.vol_id}")
      end
    end
  end

  [old_volumes, rm_old, rm_new]
end

#
# Given a set of new volumes, generate a list of changes
# to apply to the system.
#
# Initial HACK: clear and recreate if difference
# Future Hack: difference and move as need
#
def compute_delta(new_volumes, controllers)
  actions = []
  errors = []

  old_volumes, rm_old, rm_new = get_volume_differences(new_volumes, controllers)

  #
  # Old volumes should be deleted!
  #
  # If all old volumes are being removed, clear config
  #
  if rm_old.length == 0
    ccs = old_volumes.map { |v| v.controller }
    ccs.uniq.each do |c|
      actions << { :action => :clear_config, :controller => c }
    end
  else
    old_volumes.each do |v|
      next if rm_old.include? v
      actions << { :action => :delete_vd, :controller => v.controller, :volume => v }
    end
  end

  #
  # New volumes should be created!
  # 
  touched = []
  new_volumes.each do |v|
    next if rm_new.include? v
    vol_data = {
      :name => v.vol_name,
      :type => v.raid_level,
      :size => v.size,
      :disks => v.members
    }
    vol_data[:stripe_size] = v.stripe_size if v.stripe_size
    actions << { :action => :clear_foreign_config, :controller => v.controller } unless touched.include? v.controller.controller_id
    actions << { :action => :create_vd, :controller => v.controller, :volume => vol_data }
    touched << v.controller.controller_id
  end

  [ actions, errors ]
end

#
# Create, delete, clear, setboot actions
#
def do_actions(actions)
  touched_controllers  = []
  jids = []
  wsman_driver = nil
  actions.each do |action|
    controller = action[:controller]
    driver     = controller.driver

    touched_controllers << controller unless touched_controllers.include? controller

    case action[:action]
      when :create_vd
        log_action("create_vd: #{controller.controller_id} #{action[:volume][:type]} #{action[:volume][:disks].length}")
        driver.create_vd(controller, action[:volume])
      when :delete_vd
        log_action("delete_vd: #{controller.controller_id} #{action[:volume].vol_id}")
        driver.delete_vd(action[:volume])
      when :clear_config
        log_action("clear_config: #{controller.controller_id} Clear all config")
        driver.clear_controller_config(controller, :all)
      when :clear_foreign_config
        log_action("clear_config: #{controller.controller_id} Clear foreign config")
        driver.clear_controller_config(controller, :foreign)
    end
  end

  node.save # Save the log actions

  touched_controllers.each do |controller|
    driver = controller.driver
    if driver.respond_to?(:create_raid_config_job)
      wsman_driver = driver
      returnVal,job_id = driver.create_raid_config_job(controller.controller_id)
      if(returnVal == 4096)
        jids << job_id
      end
    end
  end

  ## Only way to get job id is to use the wsman driver..setup the job queue now
  if (jids and jids.length > 0)
    wsman_driver.run_multiple_raid_jobs(jids) 
  end

  []
end


def apply_config(config, controllers)
  volumes, errors = build_volumes(config, controllers)
  if errors.length > 0
    log("Config Errors:#{errors}")
    throw "Config error #{errors}"
  end

  actions, errors  = compute_delta(volumes, controllers)
  if errors.length > 0
    log("Delta Compute Errors:#{errors}")
    throw "Delta Compute error #{errors}"
  end

  # May reboot if needed
  errors = do_actions(actions)
  node.save # Just in case status info is updated.
  if errors.length > 0
    log("Action Errors:#{errors}")
    throw "Action error #{errors}"
  end
rescue Exception => e
  log(e.backtrace)
  report_problem($!)  ## $! is the global exception variable
end

#
# Setup up the boot operation.
#
def do_set_boot(config, controllers, nic_first)
  # Get new volumes
  volumes, errors = build_volumes(config, controllers)
  if errors.length > 0
    log("Config Errors:#{errors}")
    throw "Config error #{errors}"
  end
  get_volume_differences(volumes, controllers)

  #
  # Mark the selected volume bootable
  #
  boot_vol = volumes.first
  volumes.each do |v|
    next unless v.bootable
    boot_vol = v
    break
  end
  controller = boot_vol.controller
  driver = controller.driver
  driver.set_boot(controller, boot_vol, nic_first)
rescue 
  report_problem($!)  ## $! is the global exception variable
end

