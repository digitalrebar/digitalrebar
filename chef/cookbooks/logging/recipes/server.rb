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


package "rsyslog"


external_servers = node[:rebar][:logging][:external_servers]
unless node[:rebar][:logging][:servers] &&
    node[:rebar][:logging][:servers].include?(node.address.addr)
  node.set[:rebar][:logging][:servers] = [ node.address.addr ]
end

# Disable syslogd in favor of rsyslog on suse (presumably desirable
# for redhat too, but I'm not in a position to test/verify ATM - see
# client.rb for example)
case node[:platform]
  when "suse"
    ruby_block "edit sysconfig syslog" do
      block do
        rc = Chef::Util::FileEdit.new("/etc/sysconfig/syslog")
        rc.search_file_replace_line(/^SYSLOG_DAEMON=/, "SYSLOG_DAEMON=rsyslogd")
        rc.write_file
      end
    end
end

service "rsyslog" do
  case
  when File.executable?("/bin/systemctl") then provider Chef::Provider::Service::Systemd
  when node[:platform] == "ubuntu" then provider Chef::Provider::Service::Upstart
  when node[:platform] == "suse" && node[:platform_version].to_f < 12.3
    service_name "syslog"
  end
  supports :restart => true, :status => true, :reload => true
  running true
  enabled true
  action [ :enable, :start ]
end

directory "/var/log/nodes" do
  owner "root"
  group "root"
  mode "0644"
  action :create
end

directory "/etc/rsyslog.d" do
  owner "root"
  group "root"
  mode "0644"
  action :create
end

template "/etc/rsyslog.d/10-rebar-server.conf" do
  owner "root"
  group "root"
  mode 0644
  source "rsyslog.server.erb"
  variables(:external_servers => external_servers)
  notifies :restart, "service[rsyslog]"
end

# dropping privileges seems to not allow network ports < 1024.
# so, don't drop privileges.
utils_line "#don't drop user privileges to keep network\n  " do
  action :add
  regexp_exclude '\$PrivDropToUser\s+syslog\s*'
  file "/etc/rsyslog.conf"
  notifies :restart, "service[rsyslog]"
end

utils_line "#don't drop group privileges to keep network\n  " do
  action :add
  regexp_exclude '\$PrivDropToGroup\s+syslog\s*'
  file "/etc/rsyslog.conf"
  notifies :restart, "service[rsyslog]"
end
