#
# Cookbook Name:: rebar-bootstrap
# Recipe:: rebar-user
#
# Copyright (C) 2015 Greg Althaus
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
#

user "rebar" do
  home "/home/rebar"
  action :create
  password '$6$afAL.34B$T2WR6zycEe2q3DktVtbH2orOroblhR6uCdo5n3jxLsm47PBm9lwygTbv3AjcmGDnvlh0y83u2yprET8g9/mve.'
  shell "/bin/bash"
  supports :manage_home => true
end

group "rebar" do
  action :create
  append true
  members "rebar"
end

directory "/home/rebar/.ssh" do
  action :create
  owner "rebar"
  group "rebar"
  mode 0755
end

bash "Fix up /opt/digitalrebar permissions" do
  code 'cd /opt/digitalrebar; chown -R rebar:rebar .'
  not_if "su -c 'test -w /opt/digitalrebar/core' rebar"
end

bash "Regenerate Rebar SSH keys" do
  code "su -l -c 'ssh-keygen -q -b 2048 -P \"\" -f /home/rebar/.ssh/id_rsa' rebar"
  not_if "test -f /home/rebar/.ssh/id_rsa"
end

bash "Enable root access" do
  cwd "/root/.ssh"
  code <<EOC
cat authorized_keys /home/rebar/.ssh/id_rsa.pub >> authorized_keys.new
sort -u <authorized_keys.new >authorized_keys
rm authorized_keys.new
EOC
end

template "/home/rebar/.ssh/config" do
  source "ssh_config.erb"
  owner "rebar"
  group "rebar"
  mode 0644
end

template "/etc/sudoers.d/rebar" do
  source "rebar_sudoer.erb"
  mode 0440
end

