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
  proxy = node["crowbar"]["proxy"]["servers"].first
  params = node["crowbar"]["provisioner"]["server"]["boot_specs"][os]
  tftproot = node["crowbar"]["provisioner"]["server"]["root"]
  api_server = "http://#{node["crowbar"]["api"]["servers"].first}"
  ntp_server = "#{node["crowbar"]["ntp"]["servers"].first}"
  provisioner_web = "http://#{node["crowbar"]["provisioner"]["server"]["webservers"].first}"
  use_local_security = node["crowbar"]["provisioner"]["server"]["use_local_security"]
  keys = node["crowbar"]["access_keys"].values.sort
  machine_key = node["crowbar"]["machine_key"]
  os_dir = "#{tftproot}/#{os}"
  mnode_name = new_resource.name
  node_dir = "#{tftproot}/nodes/#{mnode_name}"
  web_path = "#{provisioner_web}/nodes/#{mnode_name}"
  crowbar_repo_web="#{web_path}/crowbar-extra"
  admin_web="#{web_path}/install"
  append = "cloud-config-url=#{web_path}/coreos-bootstrap-install.sh crowbar.install.key=#{machine_key}"
  v4addr = new_resource.address

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
              :web_path => web_path)
  end

  template "#{node_dir}/crowbar_join.sh" do
    mode 0644
    owner "root"
    group "root"
    source "crowbar_join.sh.erb"
    variables(:api_server => api_server,
              :ntp_server => ntp_server,
              :provisioner_web => provisioner_web)
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
