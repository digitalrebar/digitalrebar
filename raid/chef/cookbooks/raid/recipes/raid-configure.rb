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

return unless node[:raid][:enable]

config_name = node[:crowbar][:hardware][:raid_set] rescue config_name = "JBODOnly"
config_name ="raid-#{config_name}"
config_bag = data_bag("crowbar-data")
config = data_bag_item("crowbar-data",config_name) if config_bag.include?(config_name)

return if config.nil? || config.empty?

raid_config "lsi-raid-config" do
  config config["config"]
  debug_flag node[:raid][:debug]
  nic_first true
  action [ :report, :apply, :set_boot ]
end
