# Copyright 2013, Dell
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
  os = "#{new_resource.distro}-#{new_resource.version}"
  proxy = node["rebar"]["proxy"]["servers"].first["url"]
  proxy_addr = node["rebar"]["proxy"]["servers"].first["address"]
  repos = node["rebar"]["provisioner"]["server"]["repositories"][os]
  params = node["rebar"]["provisioner"]["server"]["boot_specs"][os]
  online = node["rebar"]["provisioner"]["server"]["online"]
  tftproot = node["rebar"]["provisioner"]["server"]["root"]
  provisioner_web = node["rebar"]["provisioner"]["server"]["webservers"].first["url"]
  api_server=node['rebar']['api']['servers'].first["url"]
  ntp_server = "#{node["rebar"]["ntp"]["servers"].first}"
  keys = node["rebar"]["access_keys"].values.sort.join($/)
  machine_key = node["rebar"]["machine_key"]
  mnode_name = new_resource.name
  mnode_rootdev = new_resource.rootdev
  node_dir = "#{tftproot}/nodes/#{mnode_name}"
  web_path = "#{provisioner_web}/nodes/#{mnode_name}"
  append = "ksdevice=bootif ks=#{web_path}/compute.ks #{params["kernel_params"]} rebar.fqdn=#{mnode_name} rebar.install.key=#{machine_key}"
  v4addr = new_resource.address

  directory node_dir do
    action :create
    recursive true
  end

  template "#{node_dir}/compute.ks" do
    mode 0644
    source "compute.ks.erb"
    owner "root"
    group "root"
    variables(:os_install_site => params[:os_install_site],
              :name => mnode_name,
              :rootdev => mnode_rootdev,
              :proxy => proxy,
              :proxy_addr => proxy_addr,
              :repos => repos,
              :provisioner_web => provisioner_web,
              :api_server => api_server,
              :logging_server => api_server, # GREG: FIX THis with logging service
              :online => online,
              :keys => keys,
              :web_path => web_path)
  end

  template "#{node_dir}/rebar_join.sh" do
    mode 0644
    owner "root"
    group "root"
    source "rebar_join.sh.erb"
    variables(:api_server => api_server, :ntp_server => ntp_server, :name => mnode_name)
  end

  provisioner_bootfile mnode_name do
    bootenv "#{os}-install"
    kernel params["kernel"]
    initrd params["initrd"]
    address v4addr
    kernel_params append
    action :add
  end
  
end
