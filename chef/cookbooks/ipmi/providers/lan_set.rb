# Copyright 2011, Dell
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#  http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

## utility to check if a lan parameter needs to be set 
## (if it's current value is different than desired one).
def check_ipmi_lan_value(header, desired)
  c_awk = "awk -F: '/^#{header}[ \\t]*:/ { print $2 }'"
  current = %x{ipmitool lan print 1 | #{c_awk} }
  current = current.chomp.strip
  current.casecmp(desired) == 0
end

action :run do
  name = new_resource.name
  command = new_resource.command
  value = new_resource.value
  settle_time = new_resource.settle_time

  if ::File.exists?("/sys/module/ipmi_devintf")
    unless check_ipmi_lan_value(name, value)
      # Set BMC LAN parameters
      ruby_block "bmc-set-lan-#{name}" do
        block do
          %x{#{command}}
          if $?.exitstatus == 0
            %x{sleep #{settle_time}}
            node.set["crowbar_wall"]["status"]["ipmi"]["messages"] << "#{name} set to #{value}" unless node.nil?
            node.set["crowbar_wall"]["status"]["ipmi"]["address_set"] = true if name == "IP Address"
          else
            node.set["crowbar_wall"]["status"]["ipmi"]["messages"] << "Unable to set #{name} to #{value}" unless node.nil?
          end
        end
      end
    else
      node.set["crowbar_wall"]["status"]["ipmi"]["messages"] << "#{name} already set to #{value}" unless node.nil?
      node.set["crowbar_wall"]["status"]["ipmi"]["address_set"] = true if name == "IP Address"
    end
  else
    node.set["crowbar_wall"]["status"]["ipmi"]["messages"] << "Unsupported product found #{node[:dmi][:system][:product_name]} - skipping IPMI:#{name}" unless node.nil?
  end
end
