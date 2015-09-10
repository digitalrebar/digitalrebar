# Copyright 2015, RackN
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

server_name = node['dns-mgmt']['server_name']
access_name = node['dns-mgmt']['access_name']
access_password = node['dns-mgmt']['access_password']
web_port = node['dns-mgmt']['server_port']
ip_addr = (IP.coerce(node['dns-mgmt']['server_address']).addr rescue nil)
key_pem = node['dns-mgmt']['https-key']
cert_pem = node['dns-mgmt']['https-cert']

dns_servers = node['rebar']['dns']['nameservers']

if !key_pem or !cert_pem
  my_ips = %x{ip addr | grep inet | awk '{ print $2 }' | awk -F/ '{ print $1}'}.split("\n")

  names = [ node.name ]
  if node.name.include? '.'
    names << node.name.split('.')[0]
  end

  template '/etc/dns-mgmt-cert.conf' do
    source 'dns-mgmt-cert.conf.erb'
    mode 0600
    variables(:addrs => my_ips, :names => names)
    action :create
  end

  bash 'Build keys' do
    code <<EOC
  set -e
  openssl req -nodes -sha256 -x509 -newkey rsa:2048 \
     -keyout /etc/dns-mgmt-https-key.pem -out /etc/dns-mgmt-https-cert.pem -days 1001 \
     -subj "/C=US/ST=Denial/L=Anytown/O=Dis/CN=admin" -config /etc/dns-mgmt-cert.conf
  chmod 600 /etc/dns-mgmt*
EOC
    only_if do !File.file?('/etc/dns-mgmt-https-key.pem') end
  end

  ruby_block 'Record cert' do
    block do
      node.normal['dns-mgmt']['https_cert'] = File.read('/etc/dns-mgmt-https-cert.pem')
      node.normal['dns-mgmt']['https_key'] = File.read('/etc/dns-mgmt-https-key.pem')
    end
    action :run
  end
end

file '/etc/dns-mgmt-https-key.pem' do
  owner 'rebar'
  group 'rebar'
  mode '0600'
  action :create
end

file '/etc/dns-mgmt-https-cert.pem' do
  owner 'rebar'
  group 'rebar'
  mode '0600'
  action :create
end

# Build dns mgmgt config file.
template '/etc/dns-mgmt.conf' do
  source 'dns-mgmt.conf.erb'
  mode 0600
  owner 'root'
  variables(:web_port => web_port,
            :access_name => access_name,
            :access_password => access_password,
            :services => dns_servers)
end

template '/etc/init.d/dns-mgmt' do
  mode '0755'
  source 'dns-mgmt-init.erb'
  notifies :restart, 'service[dns-mgmt]'
end

directory '/var/cache/rebar-dns-mgmt' do
  recursive true
  owner 'root'
  group 'root'
  mode 0700
  action :create
end

file '/var/cache/rebar-dns-mgmt/database.json' do
  owner 'root'
  group 'root'
  mode 0644
  content '{}'
  action :create_if_missing
end

service 'dns-mgmt' do
  action [ :enable, :start ]
end

master_acl_token=node[:consul][:acl_master_token]

ruby_block "check for dns #{server_name} management acl" do
  block do
    listStr = %x{curl http://localhost:8500/v1/acl/list?token=#{master_acl_token}}
    list = JSON.parse(listStr)
    list.each do |elem|
      if elem['Name'] == "DNS #{server_name} Management ACL"
        node.normal['consul']['tokens'] ||= {}
        node.normal['consul']['tokens']["dns_#{server_name}_mgmt_acl"] = elem['ID']
        break
      end
    end
  end
  action :run
end

bash "Create DNS #{server_name} Management ACL" do
  code <<EOC
    echo '{
      "Name": "DNS #{server_name} Management ACL",
      "Type": "client",
      "Rules": "key \\"digitalrebar/private\\" { policy = \\"deny\\" }\\nkey \\"digitalrebar/private/dns-mgmt/#{server_name}\\" { policy = \\"write\\" }"
    }' > /tmp/tmp_cred.json
    curl -X PUT -d @/tmp/tmp_cred.json http://localhost:8500/v1/acl/create?token=#{master_acl_token}
  rm -f /tmp/tmp_cred.json
EOC
  only_if do
    ((node.normal['consul']['tokens']["dns_#{server_name}_acl"] == nil or
      node.normal['consul']['tokens']["dns_#{server_name}_acl"] == {}) rescue true)
  end
end

ruby_block "record the DNS #{server_name} Management ACL" do
  block do
    listStr = %x{curl http://localhost:8500/v1/acl/list?token=#{master_acl_token}}
    list = JSON.parse(listStr)
    list.each do |elem|
      if elem['Name'] == 'DNS #{server_name} Management ACL'
        node.normal['consul']['tokens'] ||= {}
        node.normal['consul']['tokens']["dns_#{server_name}_mgmt_acl"] = elem['ID']
        break
      end
    end
  end
  action :run
end

bash "Add dns mgmt #{server_name} keys to consul" do
  code <<EOC
    curl -X PUT -d '#{access_name}' http://127.0.0.1:8500/v1/kv/digitalrebar/private/dns-mgmt/#{server_name}/access_name?token=#{master_acl_token}
    curl -X PUT -d '#{access_password}' http://127.0.0.1:8500/v1/kv/digitalrebar/private/dns-mgmt/#{server_name}/access_password?token=#{master_acl_token}
    curl -X PUT --data-binary @/etc/dns-mgmt-https-cert.pem http://127.0.0.1:8500/v1/kv/digitalrebar/private/dns-mgmt/#{server_name}/cert_pem?token=#{master_acl_token}
EOC
  # GREG: Make this conditional
end

bash 'reload consul dns-mgmt' do
  code '/usr/local/bin/consul reload'
  action :nothing
end

template '/etc/consul.d/rebar-dns-mgmt-server.json' do
  source 'consul-dns-mgmt-server.json.erb'
  mode 0600
  owner 'root'
  variables(:web_port => web_port, :ip_addr => ip_addr, :service_name => server_name)
  notifies :run, 'bash[reload consul dns-mgmt]', :immediately
end
