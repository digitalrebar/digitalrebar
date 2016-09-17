# Copyright 2014, Greg Althaus
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

#
# Install and start lldpd
#

# This is a little bit of a hack for now.
return if node[:platform] == "coreos" || node[:rebar_ohai][:in_docker]

package_name = "lldpd"
service_name = "lldpd"

case node[:platform]
when "centos"
  package_name = "lldpad" if node[:platform_version] == "6.5"
  service_name = "lldpad" if node[:platform_version] == "6.5"
when "rhel", "redhat"
  package_name = "lldpad"
  service_name = "lldpad"
end

unless system("which #{service_name}")
  case node[:platform]
  when 'ubuntu','debian'
    package package_name do
          action :install
          options '--force-yes'
    end
  else
    package package_name
  end
end

service service_name do
  action [ :enable, :start ]
end

