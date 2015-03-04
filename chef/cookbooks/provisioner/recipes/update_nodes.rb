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

domain_name = node['crowbar']['dns']['domain']
provisioner_web=node['crowbar']['provisioner']['server']['webserver']
tftproot = node['crowbar']['provisioner']['server']['root']
machine_key = node["crowbar"]["provisioner"]["machine_key"]
node_dir="#{tftproot}/nodes"
discover_dir="#{tftproot}/discovery"
pxecfg_dir="#{discover_dir}/pxelinux.cfg"
uefi_dir=discover_dir
pxecfg_default="#{pxecfg_dir}/default"
node.normal['crowbar_wall'] ||= Mash.new
node.normal['crowbar_wall']['provisioner'] ||= Mash.new
node.normal['crowbar_wall']['provisioner']['clients'] ||= Mash.new
new_clients = {}

(node['crowbar']['provisioner']['clients'] || {} rescue {}).each do |mnode_name,provisioner_info|
  # Build PXE and ELILO config files for each system
  v4addr = IP.coerce(provisioner_info["v4addr"])
  nodeaddr = sprintf("%X",v4addr.address)
  bootenv = provisioner_info["bootenv"]
  pxefile = "#{pxecfg_dir}/#{nodeaddr}"
  uefifile = "#{uefi_dir}/#{nodeaddr}.conf"
  new_clients[mnode_name] = {
    "v4addr" => provisioner_info["v4addr"],
    "nodeaddr" => nodeaddr,
    "pxefile" => pxefile,
    "uefifile" => uefifile
  }
  # Generate an appropriate control.sh for the system.
  directory "#{node_dir}/#{mnode_name}" do
    action :create
    recursive true
  end
  Chef::Log.info("PROVISIONER: #{mnode_name} Updating PXE and UEFI boot for bootenv #{bootenv}")
  # Default to creating appropriate boot config files for Sledgehammer.
  case bootenv
  when "local"
    provisioner_bootfile mnode_name do
      bootenv "sledgehammer"
      address v4addr
      action :remove
    end
  when 'sledgehammer'
    pxe_params = node['crowbar']['provisioner']['server']['sledgehammer_kernel_params'].split(' ')
    pxe_params << "crowbar.fqdn=#{mnode_name}"
    pxe_params << "crowbar.install.key=#{machine_key}"
    provisioner_bootfile mnode_name do
      kernel_params pxe_params.join(' ')
      address v4addr
      bootenv "sledgehammer"
      action :add
    end
    template "#{node_dir}/#{mnode_name}/control.sh" do
      source 'control.sh.erb'
      mode '0755'
      variables(:provisioner_name => node.name,
                :online => node['crowbar']['provisioner']['server']['online'],
                :domain => domain_name,
                :provisioner_web => provisioner_web,
                :proxy => node['crowbar']['provisioner']['server']['proxy'],
                :keys => (node['crowbar']['provisioner']['server']['access_keys'] rescue Hash.new).values.sort.join($/),
                :v4_addr => node.address('admin',IP::IP4).addr
                )
    end
  when "ubuntu-12.04-install"
    provisioner_debian mnode_name do
      distro "ubuntu"
      version "12.04"
      address v4addr
      target mnode_name
      action :add
    end
  when 'ubuntu-14.04-install'
    provisioner_debian mnode_name do
      distro 'ubuntu'
      version '14.04'
      address v4addr
      target mnode_name
      action :add
    end
  when 'centos-6.5-install'
    provisioner_redhat mnode_name do
      distro 'centos'
      version '6.5'
      address v4addr
      target mnode_name
      action :add
    end
  when 'centos-6.6-install'
    provisioner_redhat mnode_name do
      distro 'centos'
      version '6.6'
      address v4addr
      target mnode_name
      action :add
    end
  when 'redhat-6.5-install'
    provisioner_redhat mnode_name do
      distro 'redhat'
      version '6.5'
      address v4addr
      target mnode_name
      action :add
    end
  when 'redhat-6.6-install'
    provisioner_redhat mnode_name do
      distro 'redhat'
      version '6.6'
      address v4addr
      target mnode_name
      action :add
    end
  when 'fedora-20-install'
    provisioner_fedora mnode_name do
      distro 'fedora'
      version '20'
      address v4addr
      target mnode_name
      action :add
    end
  when 'redhat-7.0-install'
    provisioner_fedora mnode_name do
      distro 'redhat'
      version '7.0'
      address v4addr
      target mnode_name
      action :add
    end
  when 'centos-7.0.1406-install'
    provisioner_fedora mnode_name do
      distro 'centos'
      version '7.0.1406'
      address v4addr
      target mnode_name
      action :add
    end
  when "debian-7.8.0-install"
    provisioner_debian mnode_name do
      distro "debian"
      version "7.8.0"
      address v4addr
      target mnode_name
      action :add
    end
  else
    Chef::Log.info("Not messing with boot files for bootenv #{bootenv}")
  end
  file "#{node_dir}/#{mnode_name}/bootstate" do
    action :create
    content bootenv
  end
end

# Now that we have handled any updates we care about, delete any info about nodes we have deleted.
(node['crowbar_wall']['provisioner']['clients'].keys - new_clients.keys).each do |old_node_name|
  old_node = node['crowbar_wall']['provisioner']['clients'][old_node_name]
  a = provisioner_bootfile old_node_name do
    action :nothing
    address IP.coerce(old_node["v4addr"])
  end
  a.run_action(:remove)
  a = directory "#{node_dir}/#{old_node_name}" do
    action :nothing
    recursive true
  end
  a.run_action(:delete)
end

bash "Restore selinux contexts for #{tftproot}" do
  code "restorecon -R -F #{tftproot}"
  only_if 'which selinuxenabled && selinxenabled'
end

node.normal['crowbar_wall']['provisioner']['clients']=new_clients
