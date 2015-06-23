#
# Cookbook Name:: resolver
# Recipe:: default
#
# Copyright 2009, Opscode, Inc.
# Copyright 2011, Dell, Inc.
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

return if node[:crowbar_ohai][:in_docker]

na = (node[:crowbar][:dns][:nameservers].collect { |x| x['address'] } rescue nil)
dns_list = (na rescue nil) ||
  (node[:crowbar][:dns][:server][:forwarders] rescue [])

execute "Disable resolvconf" do
  command "resolvconf --disable-updates"
  only_if "which resolvconf &>/dev/null && resolvconf --updates-are-enabled"
end

execute "Allow changes to resolv.conf" do
  command "chattr -i /etc/resolv.conf"
  ignore_failure true
end

link "/etc/resolv.conf" do
  action :delete
  only_if do File.symlink?("/etc/resolv.conf") end
end

template "/etc/resolv.conf" do
  source "resolv.conf.erb"
  owner "root"
  group "root"
  mode 0644
  variables(:nameservers => dns_list.flatten, :search => (node[:crowbar][:dns][:domain] rescue nil))
end

execute "Lock down resolv.conf" do
  command "chattr +i /etc/resolv.conf"
end
