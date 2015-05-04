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
  provisioner_web = node["crowbar"]["provisioner"]["server"]["webservers"].first["url"]
  api_server=node['crowbar']['api']['servers'].first["url"]
  ntp_server = "#{node["crowbar"]["ntp"]["servers"].first}"
  use_local_security = node["crowbar"]["provisioner"]["server"]["use_local_security"]
  install_url=node["crowbar"]["provisioner"][""]
  machine_key = node["crowbar"]["machine_key"]
  keys = node["crowbar"]["access_keys"].values.sort.join($/)
  os_dir = "#{tftproot}/#{os}"
  install_dir = "#{os_dir}/install"
  mnode_name = new_resource.name
  node_dir = "#{tftproot}/nodes/#{mnode_name}"
  web_path = "#{provisioner_web}/nodes/#{mnode_name}"
  crowbar_repo_web="#{web_path}/crowbar-extra"
  admin_web="#{web_path}/install"

  my_ip="10.20.0.2"
  my_netmask="255.255.255.0"
  my_gateway="10.20.0.1"
  my_dns="10.20.0.1"
  my_hostname=mnode_name
  my_intf="eth1"
  #
  # GREG: Fill in ip params
  #
  append = "ksdevice=#{my_intf} forceformat=yes installdrive=sda repo=nfs:#{provisioner_addr}:#{install_dir} ks=#{web_path}/compute.ks hostname=#{my_hostname} #{params["kernel_params"]} crowbar.install.key=#{machine_key} crowbar.fqdn=#{mnode_name}"
  mac_list = new_resource.address

  directory node_dir do
    action :create
    recursive true
  end

  template "#{node_dir}/compute.ks" do
    mode 0644
    source "ks.cfg.fuel.erb"
    owner "root"
    group "root"
    variables(:provisioner_web => provisioner_web,
              :api_server => provisioner_addr,
              :keys => keys)
  end

  template "#{node_dir}/crowbar_join.sh" do
    mode 0644
    owner "root"
    group "root"
    source "crowbar_join.sh.erb"
    variables(:api_server => provisioner_addr, :ntp_server => ntp_server, :name => mnode_name)
  end

  provisioner_bootfile mnode_name do
    bootenv "#{os}-install"
    kernel params["kernel"]
    initrd params["initrd"]
    address mac_list
    kernel_params append
    action :add
  end
  
end
