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

directory "/var/run/sshd" do
  mode 0755
  owner "root"
  recursive true
end

bash "Regenerate Host SSH keys" do
  code "ssh-keygen -q -b 2048 -P '' -f /etc/ssh/ssh_host_rsa_key"
  not_if "test -f /etc/ssh/ssh_host_rsa_key"
end

bash "Unlock the root account" do
  code "while grep -q '^root:!' /etc/shadow; do usermod -U root; done"
end

# We need Special Hackery to run sshd in docker.
if File.file?("/.dockerenv")
  service "ssh" do
    service_name "sshd" if node["platform"] == "centos"
    start_command "/usr/sbin/sshd"
    stop_command "pkill -9 sshd"
    status_command "pgrep sshd"
    restart_command "pkill -9 sshd && /usr/sbin/sshd"
    action [:start]
  end
else
  service "ssh" do
    service_name "sshd" if node["platform"] == "centos"
    action [:enable, :start]
  end
end

directory "/root/.ssh" do
  action :create
  recursive true
  owner "root"
  mode 0755
end

bash "Enable root access" do
  cwd "/root/.ssh"
  code <<EOC
cat authorized_keys /home/crowbar/.ssh/id_rsa.pub >> authorized_keys.new
sort -u <authorized_keys.new >authorized_keys
rm authorized_keys.new
EOC
end

template "/etc/ssh/sshd_config" do
  source "sshd_config.erb"
  action :create
  notifies :restart, 'service[ssh]', :immediately
end

template "/etc/ssh/sshd_config" do
  source "sshd_config.erb"
  action :create
  notifies :restart, 'service[ssh]', :immediately
end

