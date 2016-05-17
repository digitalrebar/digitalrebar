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

# Determine service params
service_config = {}
service_config['data_dir'] = node[:consul][:data_dir]

copy_params = [
  :bind_addr, :datacenter, :domain, :log_level, :node_name, :advertise_addr,
  :acl_datacenter, :acl_master_token, :acl_default_policy, :acl_down_policy,
  :encrypt, :disable_remote_exec
]

case node[:consul][:service_mode]
when 'server'
  copy_params << :bootstrap_expect
  service_config['server'] = true
  service_config['retry_join'] = node[:consul][:servers] - ["[#{node[:consul][:bind_addr]}]:8301"]

  bash 'Update firewall ports' do
    ignore_failure true
    code <<-EOF
firewall-cmd --add-port 8300/tcp
firewall-cmd --add-port 8301/tcp
firewall-cmd --add-port 8302/tcp
EOF
    only_if "which firewall-cmd && [[ $(firewall-cmd --state) != '*not running*' ]]"
  end

when 'client'
  service_config['retry_join'] = node[:consul][:servers]
else
  Chef::Application.fatal! 'node[:consul][:service_mode] must be "server" or "client"'
end

if node[:consul][:serve_ui]
  service_config[:ui_dir] = node[:consul][:ui_dir]
  service_config[:client_addr] = node[:consul][:client_addr]
end

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

# Bind address is really advertise address.
service_config[:advertise_addr] = service_config[:bind_addr]
service_config.delete(:bind_addr)

template '/etc/init.d/consul' do
  source 'consul-init.erb'
  mode 0755
  variables(
            consul_binary: "#{node[:consul][:install_dir]}/consul",
            config_dir: node[:consul][:config_dir],
  )
  notifies :start, 'service[consul]'
end

bash "reload systemctl daemon" do
  code "systemctl daemon-reload"
  only_if "[[ -x /bin/systemctl ]]"
  ignore_failure true
end

service 'consul' do
  supports status: true, restart: true, reload: true
  action [:enable]
end

file node[:consul][:config_dir] + '/default.json' do
  mode 0640
  action :create
  content JSON.pretty_generate(service_config, quirks_mode: true)
  notifies :restart, 'service[consul]', :immediately
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
