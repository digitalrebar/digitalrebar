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

package "ntp" do
    action :install
end unless Kernel.system("which ntpd")

if node["roles"].include?("ntp-server")
  ntp_servers = node[:rebar][:ntp][:external_servers]
else
  ntp_servers = node[:rebar][:ntp][:servers]
end

user "ntp"

template "/etc/ntp.conf" do
  owner "root"
  group "root"
  mode 0644
  force_unlink true
  source "ntp.conf.erb"
  variables(:ntp_servers => ntp_servers)
  notifies :restart, "service[ntp]"
end

#
# Make sure the ntpdate helper is removed to speed up network restarts
# This script manages ntp for the client
#
file "/etc/network/if-up.d/ntpdate" do
  action :delete
end if ::File.exists?("/etc/network/if-up.d/ntpdate")

service "ntp" do
  service_name "ntpd" if node[:platform] =~ /^(coreos|centos|redhat|fedora)$/
  supports :restart => true, :status => true, :reload => true
  running true
  enabled true
  action [ :enable, :start ]
  # Sigh, ubuntu 15.04 and its 'not really systemd' use of systemd
  ignore_failure true if node[:platform] == "ubuntu" &&
                         File.directory?("/etc/systemd/system") &&
                         !File.exists?("/usr/lib/systemd/system/ntpd.service")
end

if node["roles"].include?("ntp-server")
  bash "reload consul ntp" do
    code "/usr/local/bin/consul reload"
    action :nothing
  end

 ip_addr = IP.coerce(node["ntp"]["service_address"]).addr

  template "/etc/consul.d/rebar-ntp.json" do
    source "consul-ntp-server.json.erb"
    mode 0644
    owner "root"
    variables(:ip_addr => ip_addr)
    notifies :run, "bash[reload consul ntp]", :immediately
  end
end

