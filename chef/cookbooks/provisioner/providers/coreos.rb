# Copyright 2014, Greg Althaus
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

action :add do
  os = "coreos"
  proxy = node["rebar"]["proxy"]["servers"].first["url"]
  proxy_addr = node["rebar"]["proxy"]["servers"].first["address"]
  params = node["rebar"]["provisioner"]["server"]["supported_oses"][os]
  tftproot = node["rebar"]["provisioner"]["server"]["root"]
  api_server=node['rebar']['api']['servers'].first["url"]
  ntp_server = "#{node["rebar"]["ntp"]["servers"].first}"
  provisioner_web = node["rebar"]["provisioner"]["server"]["webservers"].first["url"]
  keys = node["rebar"]["access_keys"].values.sort
  machine_key = node["rebar"]["machine_key"]
  mnode_name = new_resource.name
  mnode_rootdev = new_resource.rootdev
  node_dir = "#{tftproot}/nodes/#{mnode_name}"
  web_path = "#{provisioner_web}/nodes/#{mnode_name}"
  append = "cloud-config-url=#{web_path}/coreos-bootstrap-install.sh rebar.install.key=#{machine_key}"
  v4addr = new_resource.address
  kernel = "#{os}/install/#{params["kernel"]}"
  initrd = "#{os}/install/#{params["initrd"]}"
  initrd = "" unless params["initrd"] && !params["initrd"].empty?


  directory node_dir do
    action :create
    recursive true
  end

  template "#{node_dir}/coreos-bootstrap-install.sh" do
    mode 0644
    owner "root"
    group "root"
    source "coreos-bootstrap-install.sh.erb"
    variables(:web_path => web_path,
              :api_server => api_server,
              :keys => keys,
              :proxy => proxy,
              :proxy_addr => proxy_addr,
              :rootdev => mnode_rootdev,
              :provisioner_web => provisioner_web)
  end

  template "#{node_dir}/cloud-config.yaml" do
    mode 0644
    owner "root"
    group "root"
    source "cloud-config.yaml.erb"
    variables(:name => mnode_name,
              :api_server => api_server,
              :keys => keys,
              :provisioner_web => provisioner_web,
              :proxy => proxy,
              :proxy_addr => proxy_addr,
              :web_path => web_path)
  end

  template "#{node_dir}/rebar_join.sh" do
    mode 0644
    owner "root"
    group "root"
    source "rebar_join.sh.erb"
    variables(:api_server => api_server,
              :ntp_server => ntp_server,
              :provisioner_web => provisioner_web)
  end

  provisioner_bootfile mnode_name do
    bootenv "#{os}-install"
    kernel kernel
    initrd initrd
    address v4addr
    kernel_params append
    action :add
  end

end
