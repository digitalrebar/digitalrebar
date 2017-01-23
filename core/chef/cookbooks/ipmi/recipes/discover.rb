#
# Copyright (c) 2011 Dell Inc.
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
# Note : This script runs on both the admin and compute nodes.
# It intentionally ignores the bios->enable node data flag.

node.set[:ipmi][:bmc_enable] = false
return if node[:rebar_ohai][:in_docker]

include_recipe "ipmi::ipmitool"

ruby_block "discover ipmi" do
  block do
    supported = IPMI.supported?(node)
    Chef::Log.info("BMC not supported.") unless supported 
    access = supported && IPMI.ensure_access(node)
    Chef::Log.info("Could not ensure BMC support is loaded.") unless access
    if access
      mcinfo = IPMI.mc_info(node)
      Chef::Log.info("Cannot validate that systems has a BMC.") unless mcinfo.empty?
      laninfo = IPMI.laninfo(node)
      Chef::Log.info("Cannot validate that BMC is remotely accessible") unless laninfo.empty?
    end
    if access && !mcinfo.empty? && !laninfo.empty?
      Chef::Log.info("BMC on #{node[:fqdn]} is useable, marking it as enabled.")
      node.set[:rebar_wall] ||= Mash.new
      node.set[:rebar_wall][:ipmi] ||= Mash.new
      node.set[:rebar_wall][:ipmi][:bmcinfo] = mcinfo
      node.set[:rebar_wall][:ipmi][:laninfo] = laninfo
      node.set[:ipmi][:bmc_enable] = true
    end
  end
end
