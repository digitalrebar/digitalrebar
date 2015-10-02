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

domain_name = node['rebar']['dns']['domain']
provisioner_web=node['rebar']['provisioner']['server']['webservers'].first["url"]
provisioner_addr=node['rebar']['provisioner']['server']['webservers'].first["address"]
api_server=node['rebar']['api']['servers'].first["url"]
ntp_server="#{node['rebar']['ntp']['servers'].first}"
tftproot = node['rebar']['provisioner']['server']['root']
machine_key = node["rebar"]["machine_key"]
node_dir="#{tftproot}/nodes"
discover_dir="#{tftproot}/discovery"
pxecfg_dir="#{discover_dir}/pxelinux.cfg"
uefi_dir=discover_dir

node.normal['rebar_wall'] ||= Mash.new
node.normal['rebar_wall']['provisioner'] ||= Mash.new
node.normal['rebar_wall']['provisioner']['clients'] ||= Mash.new
new_clients = {}

(node['rebar']['provisioner']['clients'] || {} rescue {}).each do |mnode_name,provisioner_info|
  # Build PXE and ELILO config files for each system
  v4addr = IP.coerce(provisioner_info["v4addr"])
  nodeaddr = sprintf("%08X",v4addr.address)
  bootenv = provisioner_info["bootenv"]
  pxefile = "#{pxecfg_dir}/#{nodeaddr}"
  uefifile = "#{uefi_dir}/#{nodeaddr}.conf"
  root = provisioner_info["rootdev"].sub("/dev/","") if /-install$/.match(bootenv)
  new_clients[mnode_name] = {
    "v4addr" => provisioner_info["v4addr"],
    "nodeaddr" => nodeaddr,
    "pxefile" => pxefile,
    "uefifile" => uefifile,
    "bootenv" => bootenv
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
      bootenv "local"
      address v4addr
      action :add
    end
  when 'sledgehammer'
    pxe_params = node['rebar']['provisioner']['server']['sledgehammer_kernel_params'].split(' ')
    pxe_params << "rebar.fqdn=#{mnode_name}"
    pxe_params << "rebar.install.key=#{machine_key}"
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
                :online => node['rebar']['provisioner']['server']['online'],
                :domain => domain_name,
                :provisioner_web => provisioner_web,
                :provisioner_addr => provisioner_addr,
                :ntp_server => ntp_server,
                :proxy => node['rebar']['proxy']['servers'].first['url'],
                :keys => (node['rebar']['access_keys'] rescue Hash.new).values.sort.join($/),
                :api_server => api_server
                )
    end
  when "ubuntu-12.04-install"
    provisioner_debian mnode_name do
      distro "ubuntu"
      version "12.04"
      address v4addr
      target mnode_name
      rootdev root
      action :add
    end
  when "ubuntu-12.04.3-install"
    provisioner_debian mnode_name do
      distro "ubuntu"
      version "12.04.3"
      address v4addr
      target mnode_name
      rootdev root
      action :add
    end
  when 'ubuntu-15.04-install'
    provisioner_debian mnode_name do
      distro 'ubuntu'
      version '15.04'
      address v4addr
      target mnode_name
      rootdev root
      action :add
    end
  when 'ubuntu-14.04-install'
    provisioner_debian mnode_name do
      distro 'ubuntu'
      version '14.04'
      address v4addr
      target mnode_name
      rootdev root
      action :add
    end
  when 'centos-6.5-install'
    provisioner_redhat mnode_name do
      distro 'centos'
      version '6.5'
      address v4addr
      target mnode_name
      rootdev root
      action :add
    end
  when 'centos-6.6-install'
    provisioner_redhat mnode_name do
      distro 'centos'
      version '6.6'
      address v4addr
      target mnode_name
      rootdev root
      action :add
    end
  when 'redhat-6.5-install'
    provisioner_redhat mnode_name do
      distro 'redhat'
      version '6.5'
      address v4addr
      target mnode_name
      rootdev root
      action :add
    end
  when 'redhat-6.6-install'
    provisioner_redhat mnode_name do
      distro 'redhat'
      version '6.6'
      address v4addr
      target mnode_name
      rootdev root
      action :add
    end
  when 'fedora-20-install'
    provisioner_fedora mnode_name do
      distro 'fedora'
      version '20'
      address v4addr
      target mnode_name
      rootdev root
      action :add
    end
  when 'redhat-7.0-install'
    provisioner_fedora mnode_name do
      distro 'redhat'
      version '7.0'
      address v4addr
      target mnode_name
      rootdev root
      action :add
    end
  when 'centos-7.1.1503-install'
    provisioner_fedora mnode_name do
      distro 'centos'
      version '7.1.1503'
      address v4addr
      target mnode_name
      rootdev root
      action :add
    end
  when "coreos-install"
    provisioner_coreos mnode_name do
      distro 'coreos'
      version 'latest'
      address v4addr
      target mnode_name
      rootdev root
      action :add
    end
  when "debian-7.8.0-install"
    provisioner_debian mnode_name do
      distro "debian"
      version "7.8.0"
      address v4addr
      target mnode_name
      rootdev root
      action :add
    end
  when "debian-8.1.0-install"
    provisioner_debian mnode_name do
      distro "debian"
      version "8.1.0"
      address v4addr
      target mnode_name
      rootdev root
      action :add
    end
  when "xenserver-6.5-install"
    provisioner_xenserver mnode_name do
      distro "xenserver"
      version "6.5"
      address v4addr
      target mnode_name
      rootdev root
      action :add
    end
  when "esxi-5.5-install"
    provisioner_esxi mnode_name do
      distro "esxi"
      version "5.5"
      address v4addr
      target mnode_name
      rootdev root
      action :add
    end
  when "fuel-6.0-install"
    provisioner_fuel mnode_name do
      distro "fuel"
      version "6.0"
      address v4addr
      target mnode_name
      rootdev root
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
(node['rebar_wall']['provisioner']['clients'].keys - new_clients.keys).each do |old_node_name|
  old_node = node['rebar_wall']['provisioner']['clients'][old_node_name]
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

node.normal['rebar_wall']['provisioner']['clients']=new_clients
