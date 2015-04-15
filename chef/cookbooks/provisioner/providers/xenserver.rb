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
  proxy = node["crowbar"]["proxy"]["servers"].first
  repos = node["crowbar"]["provisioner"]["server"]["repositories"][os]
  params = node["crowbar"]["provisioner"]["server"]["boot_specs"][os]
  online = node["crowbar"]["provisioner"]["server"]["online"]
  tftproot = node["crowbar"]["provisioner"]["server"]["root"]
  provisioner_web = "http://#{node["crowbar"]["provisioner"]["server"]["webservers"].first}"
  api_server = "http://#{node["crowbar"]["api"]["servers"].first}"
  ntp_server = "#{node["crowbar"]["ntp"]["servers"].first}"
  use_local_security = node["crowbar"]["provisioner"]["server"]["use_local_security"]
  install_url=node["crowbar"]["provisioner"][""]
  machine_key = node["crowbar"]["machine_key"]
  keys = node["crowbar"]["access_keys"].values.sort.join($/)
  os_dir = "#{tftproot}/#{os}"
  mnode_name = new_resource.name
  node_dir = "#{tftproot}/nodes/#{mnode_name}"
  web_path = "#{provisioner_web}/nodes/#{mnode_name}"
  crowbar_repo_web="#{web_path}/crowbar-extra"
  admin_web="#{web_path}/install"

  append = "\
       ../#{os}/install/boot/xen.gz dom0_max_vcpus=1-2 dom0_mem=752M,max:752M com1=115200,8n1 \
       console=com1,vga --- ../#{os}/install/boot/vmlinuz \
       xencons=hvc console=hvc0 console=tty0 crowbar.fqdn=#{mnode_name} crowbar.install.key=#{machine_key} \
       answerfile=#{web_path}/compute.ks #{params["kernel_params"]} \
       install --- ../#{os}/install/install.img"

  v4addr = new_resource.address

  directory node_dir do
    action :create
    recursive true
  end

  template "#{node_dir}/compute.ks" do
    mode 0644
    source "compute.ks.xen.erb"
    owner "root"
    group "root"
    variables(:os_install_site => params[:os_install_site],
              :name => mnode_name,
              :proxy => proxy,
              :repos => repos,
              :provisioner_web => provisioner_web,
              :source => "#{provisioner_web}/#{os}/install",
              :api_server => api_server,
              :online => online,
              :keys => keys,
              :web_path => web_path)
  end

  template "#{node_dir}/post-install.sh" do
    mode 0644
    owner "root"
    group "root"
    source "xen-post-install.sh.erb"
    variables(:api_server => api_server,
              :keys => keys,
              :provisioner_web => provisioner_web)
  end

  provisioner_bootfile mnode_name do
    bootenv "#{os}-install"
    kernel params["kernel"]
    initrd ""
    address v4addr
    kernel_params append
    action :add
  end

end
