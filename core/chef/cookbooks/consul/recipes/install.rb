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


install_arch = node[:kernel][:machine] =~ /x86_64/ ? 'amd64' : '386'
install_version = [node[:consul][:version], node[:os], install_arch].join('_')
install_checksum = node[:consul][:checksums].fetch(install_version)

package "unzip"

bash "Fetch consul for #{install_version}" do
  code <<EOC
set -e
d=$(mktemp -d /tmp/consul-XXXXXX)
cd $d
curl -f -L -O '#{node[:consul][:base_url]}/#{node[:consul][:version]}/consul_#{install_version}.zip'
echo '#{install_checksum}  consul_#{install_version}.zip' > sha256sums
sha256sum -c --status sha256sums
unzip "consul_#{install_version}.zip"
mv consul #{node[:consul][:install_dir]}
EOC
  not_if { ::File.exists?("#{node[:consul][:install_dir]}/consul") }
end

# Configure directories
consul_directories = []
consul_directories << node[:consul][:config_dir]
consul_directories << '/var/lib/consul'
consul_group = node[:consul][:service_group]
# Select service user & group
case node[:consul][:init_style]
when 'runit'
  consul_user = node[:consul][:service_user]
  consul_directories << '/var/log/consul'
else
  consul_user = 'root'
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
    mode 02755
  end
end
