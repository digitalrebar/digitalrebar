#
# Cookbook Name:: crowbar-bootstrap
# Recipe:: crowbar-user
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

user "crowbar" do
  home "/home/crowbar"
  action :create
  password '$6$afAL.34B$T2WR6zycEe2q3DktVtbH2orOroblhR6uCdo5n3jxLsm47PBm9lwygTbv3AjcmGDnvlh0y83u2yprET8g9/mve.'
  shell "/bin/bash"
  supports :manage_home => true
end

group "crowbar" do
  action :create
  append true
  members "crowbar"
end

directory "/home/crowbar/.ssh" do
  action :create
  owner "crowbar"
  group "crowbar"
  mode 0755
end

bash "Fix up /opt/opencrowbar permissions" do
  code 'cd /opt/opencrowbar; chown -R crowbar:crowbar .'
  not_if "su -c 'test -w /opt/opencrowbar/core' crowbar"
end

bash "Regenerate Crowbar SSH keys" do
  code "su -l -c 'ssh-keygen -q -b 2048 -P \"\" -f /home/crowbar/.ssh/id_rsa' crowbar"
  not_if "test -f /home/crowbar/.ssh/id_rsa"
end

bash "Enable root access" do
  cwd "/root/.ssh"
  code <<EOC
cat authorized_keys /home/crowbar/.ssh/id_rsa.pub >> authorized_keys.new
sort -u <authorized_keys.new >authorized_keys
rm authorized_keys.new
EOC
end

template "/home/crowbar/.ssh/config" do
  source "ssh_config.erb"
  owner "crowbar"
  group "crowbar"
  mode 0644
end

template "/etc/sudoers.d/crowbar" do
  source "crowbar_sudoer.erb"
  mode 0440
end

