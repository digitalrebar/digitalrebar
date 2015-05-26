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
  raise "Fuel Broken for now until it is ported to use the new disk reservation code!"
  os = "#{new_resource.distro}-#{new_resource.version}"
  params = node["crowbar"]["provisioner"]["server"]["boot_specs"][os]
  tftproot = node["crowbar"]["provisioner"]["server"]["root"]
  provisioner_web = node["crowbar"]["provisioner"]["server"]["webservers"].first["url"]
  ntp_server = "#{node["crowbar"]["ntp"]["servers"].first}"
  machine_key = node["crowbar"]["machine_key"]
  keys = node["crowbar"]["access_keys"].values.sort.join($/)
  os_dir = "#{tftproot}/#{os}"
  install_dir = "#{os_dir}/install"
  mnode_name = new_resource.name
  mnode_rootdev = new_resource.rootdev
  node_dir = "#{tftproot}/nodes/#{mnode_name}"
  web_path = "#{provisioner_web}/nodes/#{mnode_name}"
  my_intf="eth1"
  #
  # GREG: Fill in ip params
  #
  append = "ksdevice=#{my_intf} forceformat=yes installdrive=sda repo=nfs:#{provisioner_addr}:#{install_dir} ks=#{web_path}/compute.ks hostname=#{mnode_name} #{params["kernel_params"]} crowbar.install.key=#{machine_key} crowbar.fqdn=#{mnode_name}"
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
              :keys => keys,
              :rootdev => mnode_rootdev)
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
