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


##
# this recipe performs substantially the same activites as the raid-configure, short
# of actually attempting to configure the RAID controller.
# rather, it reports information about the current configuration and the physical
# capabilities of the storage system.

include_recipe "utils"

raid_enable = node[:raid][:enable] & @@centos
log("BEGIN raid-report enabled=#{raid_enable}") {level :info} 

config_name = node[:crowbar][:hardware][:raid_set] rescue config_name = "JBODOnly"
config_name ="raid-#{config_name}"
config_bag = data_bag("crowbar-data")
config = data_bag_item("crowbar-data",config_name) if config_bag.include?(config_name)
log("Using config: #{config_name}")
begin 
  ## push the MegaCLI packages, and insall them
  include_recipe "raid::install_tools"
  
  raid_raid_config "lsi-raid-report" do
    config config["config"]
    debug_flag   node[:raid][:debug]  
    action [ :report ]
    problem_file "/var/log/chef/hw-problem.log"
  end
  node.save
end if raid_enable and !config.nil? and !config.empty?
log("END raid-report") {level :info} 
