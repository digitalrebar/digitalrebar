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

include_recipe 'utils'
directory '/etc/bind'

ip_addr = (IP.coerce(node['dns']['service_address']).addr rescue nil)
server_name = node['rebar']['dns']['service_name']

case node[:platform]
when 'redhat','centos'
  template '/etc/sysconfig/named' do
    source 'redhat-sysconfig-named.erb'
    mode 0644
    owner 'root'
    variables :options => { 'OPTIONS' => '-c /etc/named.conf' }
  end
when 'suse'
  template '/etc/sysconfig/named' do
    source 'suse-sysconfig-named.erb'
    mode 0644
    owner 'root'
    variables :options => { 'NAMED_ARGS' => '-c /etc/named.conf' }
  end
end

service 'bind9' do
  case node[:platform]
  when 'centos','redhat','suse','opensuse'
    service_name 'named'
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
    when 'ubuntu','debian' then group 'bind'
    when 'centos','redhat','suse','opensuse' then group 'named'
    end
    mode 0644
    owner 'root'
    notifies :reload, 'service[bind9]'
  end
end

# If we don't have a local named.conf.local, create one.
# We keep this around to let local users add stuff to
# DNS that Rebar will not manage.
# We also create a named.conf.rebar if it does not exist to
# keep bind happy before we start creating nodes.

%w[local rebar].each do |z|
  bash "/etc/bind/named.conf.#{z}" do
    code "touch /etc/bind/named.conf.#{z}"
    not_if { ::File.exists? "/etc/bind/named.conf.#{z}" }
  end
end

# Rewrite our default configuration file
template '/etc/named.conf' do
  source 'named.conf.erb'
  mode 0644
  owner 'root'
  case node[:platform]
  when 'ubuntu','debian' then group 'bind'
  when 'centos','redhat','suse','opensuse' then group 'named'
  end
  variables(:forwarders => node[:rebar][:dns][:forwarders])
  notifies :restart, 'service[bind9]', :immediately
end

master_acl_token=node[:consul][:acl_master_token]

ruby_block "check for dns #{server_name} server acl" do
  block do
    listStr = %x{curl http://localhost:8500/v1/acl/list?token=#{master_acl_token}}
    list = JSON.parse(listStr)
    list.each do |elem|
      if elem['Name'] == "DNS #{server_name} Server ACL"
        node.normal['consul']['tokens'] ||= {}
        node.normal['consul']['tokens']["dns_#{server_name}_acl"] = elem['ID']
        break
      end
    end
  end
  action :run
end

bash "Create dns #{server_name} Server ACL" do
  code <<EOC
    echo '{
      "Name": "DNS #{server_name} Server ACL",
      "Type": "client",
      "Rules": "key \\"digitalrebar/private\\" { policy = \\"deny\\" }\\nkey \\"digitalrebar/private/dns/#{server_name}\\" { policy = \\"write\\" }"
    }' > /tmp/tmp_cred.json
    curl -X PUT --data-binary @/tmp/tmp_cred.json http://localhost:8500/v1/acl/create?token=#{master_acl_token}
  rm -f /tmp/tmp_cred.json
EOC
  only_if do
    ((node.normal['consul']['tokens']["dns_#{server_name}_acl"] == nil or
      node.normal['consul']['tokens']["dns_#{server_name}_acl"] == {}) rescue true)
  end
end

ruby_block "record the dns #{server_name} server acl" do
  block do
    listStr = %x{curl http://localhost:8500/v1/acl/list?token=#{master_acl_token}}
    list = JSON.parse(listStr)
    list.each do |elem|
      if elem['Name'] == 'DNS #{server_name} Server ACL'
        node.normal['consul']['tokens'] ||= {}
        node.normal['consul']['tokens']["dns_#{server_name}_acl"] = elem['ID']
        break
      end
    end
  end
  action :run
end

bash "Add dns server #{server_name} keys to consul" do
  code <<EOC
    curl -X PUT -d 'BIND' http://127.0.0.1:8500/v1/kv/digitalrebar/private/dns/#{server_name}/type?token=#{master_acl_token}
EOC
  # GREG: Make this conditional
end

bash 'reload consul' do
  code '/usr/local/bin/consul reload'
  action :nothing
end

template '/etc/consul.d/rebar-dns.json' do
  source 'consul-dns-server.json.erb'
  mode 0644
  owner 'root'
  variables(:ip_addr => ip_addr, :service_name => server_name)
  notifies :run, 'bash[reload consul]', :immediately
end

