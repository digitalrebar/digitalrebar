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

include_recipe "utils"
directory "/etc/bind"


case node[:platform]
when "redhat","centos"
  template "/etc/sysconfig/named" do
    source "redhat-sysconfig-named.erb"
    mode 0644
    owner "root"
    variables :options => { "OPTIONS" => "-c /etc/named.conf" }
  end
when "suse"
  template "/etc/sysconfig/named" do
    source "suse-sysconfig-named.erb"
    mode 0644
    owner "root"
    variables :options => { "NAMED_ARGS" => "-c /etc/named.conf" }
  end
end

service "bind9" do
  case node[:platform]
  when "centos","redhat","suse","opensuse"
    service_name "named"
  end
  supports :restart => true, :status => true, :reload => true
  running true
  enabled true
  action :enable
end

# Load up our default zones.  These never change.
files=%w{db.0 db.255 named.conf.default-zones}
files.each do |file|
  template "/etc/bind/#{file}" do
    source "#{file}.erb"
    case node[:platform]
    when "ubuntu","debian" then group "bind"
    when "centos","redhat","suse","opensuse" then group "named"
    end
    mode 0644
    owner "root"
    notifies :reload, "service[bind9]"
  end
end

# If we don't have a local named.conf.local, create one.
# We keep this around to let local users add stuff to
# DNS that Crowbar will not manage.
# We also create a named.conf.crowbar if it does not exist to
# keep bind happy before we start creating nodes.

%w[local crowbar].each do |z|
  bash "/etc/bind/named.conf.#{z}" do
    code "touch /etc/bind/named.conf.#{z}"
    not_if { ::File.exists? "/etc/bind/named.conf.#{z}" }
  end
end

# Rewrite our default configuration file
template "/etc/named.conf" do
  source "named.conf.erb"
  mode 0644
  owner "root"
  case node[:platform]
  when "ubuntu","debian" then group "bind"
  when "centos","redhat","suse","opensuse" then group "named"
  end
  variables(:forwarders => node[:crowbar][:dns][:forwarders])
  notifies :restart, "service[bind9]", :immediately
end

bash "reload consul" do
  code "/usr/local/bin/consul reload"
  action :nothing
end

template "/etc/consul.d/crowbar-dns.json" do
  source "consul-dns-server.json.erb"
  mode 0644
  owner "root"
  notifies :run, "bash[reload consul]", :immediately
end

