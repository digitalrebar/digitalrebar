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

# Common BIOS methods which get executed on both the install and update
# phase.

include_recipe "utils"

node["crowbar_wall"] = {} if node["crowbar_wall"].nil?
node["crowbar_wall"]["status"] = {} if node["crowbar_wall"]["status"].nil?
node["crowbar_wall"]["status"]["bios"] = []

@@debug = node[:bios][:debug]

centos = ubuntu = false
platform = node[:platform]
case platform
  when "centos", "redhat"
  centos = true
  when "ubuntu"
  ubuntu = true
end

log("BIOS: running on OS:[#{platform}] on #{node[:dmi][:system][:product_name]} hardware") { level :info} 


## enforce platfrom limitations
@@bios_setup_enable = node[:bios][:bios_setup_enable] & centos & !@@is_admin
@@bios_update_enable = node[:bios][:bios_update_enable] & centos & !@@is_admin
@@bmc_update_enable = node[:bios][:bmc_update_enable] & centos & !@@is_admin


node["crowbar_wall"]["status"]["bios"] << "Bios Barclamp using centos:#{centos} ubuntu:#{ubuntu}"
node["crowbar_wall"]["status"]["bios"] << "Bios Barclamp using setup_enabled = #{@@bios_setup_enable}"
node["crowbar_wall"]["status"]["bios"] << "Bios Barclamp using bios_update_enabled = #{@@bios_update_enable}"
node["crowbar_wall"]["status"]["bios"] << "Bios Barclamp using bmc_update_enabled = #{@@bmc_update_enable}"
node.save

