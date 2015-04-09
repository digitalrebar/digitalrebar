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
  os = "#{new_resource.distro}-#{new_resource.version}"
  repos = node["crowbar"]["provisioner"]["server"]["repositories"][os]
  params = node["crowbar"]["provisioner"]["server"]["boot_specs"][os]
  online = node["crowbar"]["provisioner"]["server"]["online"]
  tftproot = node["crowbar"]["provisioner"]["server"]["root"]
  provisioner_addr = node["crowbar"]["provisioner"]["server"]["v4addr"]
  provisioner_web = node["crowbar"]["provisioner"]["server"]["webserver"]
  use_local_security = node["crowbar"]["provisioner"]["server"]["use_local_security"]
  install_url=node["crowbar"]["provisioner"][""]
  machine_key = node["crowbar"]["provisioner"]["machine_key"]
  keys = node["crowbar"]["provisioner"]["server"]["access_keys"].values.sort.join($/)
  os_dir = "#{tftproot}/#{os}"
  install_dir = "#{os_dir}/install"
  mnode_name = new_resource.name
  node_dir = "#{tftproot}/nodes/#{mnode_name}"
  web_path = "#{provisioner_web}/nodes/#{mnode_name}"
  crowbar_repo_web="#{web_path}/crowbar-extra"
  admin_web="#{web_path}/install"

  append = "-c ../#{os}/install/boot.cfg #{params["kernel_params"]} ks=#{web_path}/compute.ks crowbar.install.key=#{machine_key}"
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
              :admin_ip => provisioner_addr,
              :keys => keys)
  end

  template "#{node_dir}/crowbar_join.sh" do
    mode 0644
    owner "root"
    group "root"
    source "crowbar_join.sh.erb"
    variables(:admin_ip => provisioner_addr)
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
