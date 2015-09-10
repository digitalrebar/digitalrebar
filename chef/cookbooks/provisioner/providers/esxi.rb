# Copyright 2015, Greg Althaus
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
  raise "ESXi broken until it os ported to use the new disk reservation code"
  os = "#{new_resource.distro}-#{new_resource.version}"
  params = node["rebar"]["provisioner"]["server"]["boot_specs"][os]
  tftproot = node["rebar"]["provisioner"]["server"]["root"]
  provisioner_web = node["rebar"]["provisioner"]["server"]["webservers"].first["url"]
  api_server=node['rebar']['api']['servers'].first["url"]
  ntp_server = "#{node["rebar"]["ntp"]["servers"].first}"
  machine_key = node["rebar"]["machine_key"]
  keys = node["rebar"]["access_keys"].values.sort.join($/)
  mnode_name = new_resource.name
  mnode_rootdev = new_resource.rootdev
  node_dir = "#{tftproot}/nodes/#{mnode_name}"
  web_path = "#{provisioner_web}/nodes/#{mnode_name}"

  append = "-c ../#{os}/install/boot.cfg #{params["kernel_params"]} ks=#{web_path}/compute.ks rebar.install.key=#{machine_key}"
  mac_list = new_resource.address

  directory node_dir do
    action :create
    recursive true
  end

  template "#{node_dir}/compute.ks" do
    mode 0644
    source "ks.cfg.esxi.erb"
    owner "root"
    group "root"
    variables(:provisioner_web => provisioner_web,
              :api_server => api_server,
              :keys => keys,
              :rootdev => mnode_rootdev)
  end

  template "#{node_dir}/rebar_join.sh" do
    mode 0644
    owner "root"
    group "root"
    source "rebar_join.sh.erb"
    variables(:api_server => api_server,
              :ntp_server => ntp_server,
              :name => mnode_name)
  end

  provisioner_bootfile mnode_name do
    bootenv "#{os}-install"
    kernel params["kernel"]
    initrd ""
    address mac_list
    kernel_params append
    action :add
  end

end
