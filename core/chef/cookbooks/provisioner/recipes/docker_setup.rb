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

api_server = node['rebar']['api']['servers'].first['url']
proxy = node['rebar']['proxy']['servers'].first['url']
tftproot = node["rebar"]["provisioner"]["server"]["root"]
node_dir="#{tftproot}/nodes"
node.normal["rebar_wall"] ||= Mash.new
node.normal["rebar_wall"]["docker"] ||= Mash.new
node.normal["rebar_wall"]["docker"]["clients"] ||= Mash.new

# Split out the v4 addresses
v4dns, v6dns = node["rebar"]["dns"]["nameservers"].collect{|a|IP.coerce(a['address'])}.partition{|a|a.v4?}
v4addresses = v4dns.collect{|a|a.addr}

(node["rebar"]["docker"]["clients"] || {} rescue {}).each do |name,info|
  # Generate an appropriate rebar init for the system
  directory "#{node_dir}/#{name}" do
    action :create
    recursive true
  end
  template "#{node_dir}/#{name}/rebar-init" do
    source "docker-node.sh.erb"
    mode 0755
    variables(:addresses => info["addresses"],
              :image => info["image"],
              :dns_servers => v4addresses,
              :name => name,
              :proxy => proxy,
              :keys => (node["rebar"]["access_keys"] rescue Hash.new).values.sort.join($/),
              :machine_key => node["rebar"]["machine_key"],
              :admin_url => api_server
              )
  end
end
