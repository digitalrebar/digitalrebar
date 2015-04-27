# Copyright 2014, Opencrowbar Team
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License
# You may obtain a copy of the License at
#
#  http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied
# See the License for the specific language governing permissions and
# limitations under the License
#
# This recipe sets up Squid

localnets = ["127.0.0.1","localhost","::1"]
`ip -o addr show`.lines.each do |line|
  next unless /inet6? ([^ ]+)/ =~ line
  localnets << IP.coerce($1).network.to_s
end
localnets.sort!

proxy_port = (node["crowbar"]["proxy"]["server"]["port"] rescue 8123) || 8123

upstream_proxy = (node["crowbar"]["proxy"]["server"]["upstream_proxy"] rescue nil)
upstream_proxy_address = nil
upstream_proxy_port = nil

if upstream_proxy && !upstream_proxy.empty?
  matchers = /http:\/\/(?<address>(\[[0-9a-f:]+\])|([^:]+)):(?<port>[0-9]+)/.match(upstream_proxy)
  raise "Could not parse upstream proxy!" unless matchers["address"]
  upstream_proxy_address = matchers["address"]
  upstream_proxy_port = matchers["port"] || 80
end

package "squid" do
  action :install
end

proxy_cache_dir = "/var/cache/squid"

user "squid" do
  action :create
  system true
end

group "squid" do
  action :create
  system true
  members "squid"
end

template "/etc/squid/squid.conf" do
  action :create
  owner "squid"
  group "squid"
  notifies :reload, "service[squid]"
  variables(:user => "squid",
            :localname => node.name,
            :localnets => localnets,
            :port => proxy_port,
            :cache_dir => proxy_cache_dir,
            :upstream_address => upstream_proxy_address,
            :upstream_port => upstream_proxy_port)
end

directory proxy_cache_dir do
  action :create
  recursive true
  owner "squid"
  group "squid"
end

bash "Initialize squid directory" do
  code "squid -z"
  not_if "test -f #{proxy_cache_dir}/swap.state"
end

bash "Increase squid start timeout" do
  code "echo SQUID_PIDFILE_TIMEOUT=60 >>/etc/sysconfig/squid"
  only_if "test -f /etc/sysconfig/squid"
  not_if "grep -q SQUID_PIDFILE_TIMEOUT /etc/sysconfig/squid"
end

service "squid" do
  reload_command "squid -k reconfigure || :"
  action [:enable, :start]
end

bash "reload consul" do
    code "/usr/local/bin/consul reload"
      action :nothing
end

template "/etc/consul.d/crowbar-squid.json" do
  source "consul-squid-server.json.erb"
  mode 0644
  owner "root"
  variables(:port => proxy_port)
  notifies :run, "bash[reload consul]", :immediately
end
