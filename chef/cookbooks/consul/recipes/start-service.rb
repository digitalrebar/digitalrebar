#
# Copyright 2014 John Bellone <jbellone@bloomberg.net>
# Copyright 2014 Bloomberg Finance L.P.
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

require 'json'

# Configure directories
consul_directories = []
consul_directories << node[:consul][:config_dir]
consul_directories << '/var/lib/consul'

# Select service user & group
case node[:consul][:init_style]
when 'runit'
  consul_user = node[:consul][:service_user]
  consul_group = node[:consul][:service_group]
  consul_directories << '/var/log/consul'
else
  consul_user = 'root'
  consul_group = 'root'
end

# Create service user
user "consul service user: #{consul_user}" do
  not_if { consul_user == 'root' }
  username consul_user
  home '/dev/null'
  shell '/bin/false'
  comment 'consul service user'
end

# Create service group
group "consul service group: #{consul_group}" do
  not_if { consul_group == 'root' }
  group_name consul_group
  members consul_user
  append true
end

# Create service directories
consul_directories.each do |dirname|
  directory dirname do
    owner consul_user
    group consul_group
    mode 0755
  end
end

# Determine service params
service_config = {}
service_config['data_dir'] = node[:consul][:data_dir]

case node[:consul][:service_mode]
when 'bootstrap'
  service_config['server'] = true
  service_config['bootstrap'] = true
when 'server'
  service_config['server'] = true
  service_config['start_join'] = node[:consul][:servers]
when 'client'
  service_config['start_join'] = node[:consul][:servers]
else
  Chef::Application.fatal! 'node[:consul][:service_mode] must be "bootstrap", "server", or "client"'
end

if node[:consul][:serve_ui]
  service_config[:ui_dir] = node[:consul][:ui_dir]
  service_config[:client_addr] = node[:consul][:client_addr]
end

copy_params = [
  :bind_addr, :datacenter, :domain, :log_level, :node_name, :advertise_addr,
  :acl_datacenter, :acl_master_token, :acl_default_policy, :acl_down_policy,
  :encrypt, :disable_remote_exec
]
copy_params.each do |key|
  if node[:consul][key]
    service_config[key] = node[:consul][key]
  end
end

# Check if we are running and our bind address changed.
bind_addr=%x{consul members | grep `hostname` | awk '{ print $2 }'}
if bind_addr && bind_addr != ""
  # bind_addr is ip:port where ip is either v4:port or [ipv6]:port
  # We need to chop off the port and the [] if there.
  bind_addr = bind_addr[0..(bind_addr.rindex(':')-1)]
  bind_addr = bind_addr[1..-2] if bind_addr[0] == '['

  if bind_addr != node[:consul][:bind_addr]
    bash 'leave cluster to rebind' do
      code "#{node[:consul][:install_dir]}/consul leave"
      only_if "#{node[:consul][:install_dir]}/consul info"
    end
  end
end

if node[:consul][:acl_master_token]
  #
  # Add this file so that we can handle
  # a restart of the crowbar service between
  # the consul node role and the crowbar-api
  # server node role on the same singleon system.
  #
  template "/etc/crowbar.master.acl" do
    source "acl-master-token.erb"
    mode 0600
    owner "crowbar"
    variables(:token => node['consul']['acl_master_token'])
  end
end

file node[:consul][:config_dir] + '/default.json' do
  user consul_user
  group consul_group
  mode 0600
  action :create
  content JSON.pretty_generate(service_config, quirks_mode: true)
end

template '/etc/init.d/consul' do
  source 'consul-init.erb'
  mode 0755
  variables(
            consul_binary: "#{node[:consul][:install_dir]}/consul",
            config_dir: node[:consul][:config_dir],
            )
  notifies :restart, 'service[consul]', :immediately
end

service 'consul' do
  supports status: true, restart: true, reload: true
  action [:enable, :start]
  subscribes :restart, "file[#{node[:consul][:config_dir]}/default.json]", :delayed
end

# Wait for consul leader to emerge.
ruby_block "wait for consul leader" do
  block do
    answer = ""
    count = 0
    # We need consul to converge on a leader.
    # This can take a little time, so we ask for
    # leader status.  The leader status returns
    # nothing, a string that says no leader, or
    # the leader IP:port pair.  The default port
    # is 8300 for server communication.
    while !(answer =~ /:8300/) and count < 30
      sleep 1
      count += 1
      answer = %x{curl http://localhost:8500/v1/status/leader}
    end
    (count >= 30 ? false : true)
  end
  action :run
end

if node[:consul][:acl_master_token]
  # Update anonymous user to prevent access the private space
  # One day we may want to require tokens for all, but ..
  bash "Update anonymous ACL" do
    code <<EOC
echo '{
  "ID": "anonymous",
  "Type": "client",
  "Rules": "key \\"opencrowbar/private\\" { policy = \\"deny\\" }"
}' > /tmp/tmp_cred.json
EOF
curl -X PUT -d @/tmp/tmp_cred.json http://localhost:8500/v1/acl/update?token=#{node[:consul][:acl_master_token]}
rm -f /tmp/tmp_cred.json
EOC
  end

  # Update anonymous user to prevent access the private space
  # One day we may want to require tokens for all, but ..
  ruby_block "check for database acl" do
    block do
      listStr = %x{curl http://localhost:8500/v1/acl/list?token=#{node[:consul][:acl_master_token]}}
      list = JSON.parse(listStr)
      found = false
      list.each do |elem|
        if elem['Name'] == 'OpenCrowbar Database ACL'
          node.normal['consul']['tokens'] ||= {}
          node.normal['consul']['tokens']['opencrowbar_database'] = elem['ID']
          break
        end
      end
    end
    action :run
  end

  bash "Create database ACL" do
    code <<EOC
echo '{
  "Name": "OpenCrowbar Database ACL",
  "Type": "client",
  "Rules": "key \\"opencrowbar/private\\" { policy = \\"deny\\" }\\nkey \\"opencrowbar/private/database/opencrowbar\\" { policy = \\"write\\" }"
}' > /tmp/tmp_cred.json
curl -X PUT -d @/tmp/tmp_cred.json http://localhost:8500/v1/acl/create?token=#{node[:consul][:acl_master_token]}
rm -f /tmp/tmp_cred.json
EOC
    only_if do
      ((node.normal['consul']['tokens']['opencrowbar_database'] == nil or
        node.normal['consul']['tokens']['opencrowbar_database'] == {}) rescue true)
    end
  end

  ruby_block "record the database acl" do
    block do
      listStr = %x{curl http://localhost:8500/v1/acl/list?token=#{node[:consul][:acl_master_token]}}
      list = JSON.parse(listStr)
      found = false
      list.each do |elem|
        if elem['Name'] == 'OpenCrowbar Database ACL'
          node.normal['consul']['tokens'] ||= {}
          node.normal['consul']['tokens']['opencrowbar_database'] = elem['ID']
          break
        end
      end
    end
    action :run
  end

end
