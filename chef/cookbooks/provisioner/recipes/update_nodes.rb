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

domain_name = node["crowbar"]["dns"]["domain"]
web_port = node["crowbar"]["provisioner"]["server"]["web_port"]
use_local_security = node["crowbar"]["provisioner"]["server"]["use_local_security"]
provisioner_web=node["crowbar"]["provisioner"]["server"]["webserver"]
provisioner_addr = node["crowbar"]["provisioner"]["server"]["v4addr"]
provisioner_port = node["crowbar"]["provisioner"]["server"]["web_port"] 
proxy=node["crowbar"]["provisioner"]["server"]["proxy"]
os_token="#{node[:platform]}-#{node[:platform_version]}"
tftproot = node["crowbar"]["provisioner"]["server"]["root"]
discover_dir="#{tftproot}/discovery"
node_dir="#{tftproot}/nodes"
node.normal["crowbar_wall"] ||= Mash.new
node.normal["crowbar_wall"]["dhcp"] ||= Mash.new
node.normal["crowbar_wall"]["dhcp"]["clients"] ||= Mash.new
new_clients = {}

(node["crowbar"]["dhcp"]["clients"] || {} rescue {}).each do |mnode_name,dhcp_info|
  # Build DHCP, PXE, and ELILO config files for each system
  v4addr = IP.coerce(dhcp_info["v4addr"])
  nodeaddr = sprintf("%X",v4addr.address)
  bootenv = dhcp_info["bootenv"]
  mac_list = dhcp_info["mac_addresses"]
  new_clients[mnode_name] = {
    "v4addr" => dhcp_info["v4addr"],
    "nodeaddr" => nodeaddr,
    "mac_addresses" => mac_list,
  }
  # Generate an appropriate control.sh for the system.
  directory "#{node_dir}/#{mnode_name}" do
    action :create
    recursive true
  end
  Chef::Log.info("DHCP: #{mnode_name} Updating PXE and UEFI boot for bootenv #{bootenv}")
  # Default to creating appropriate boot config files for Sledgehammer.
  case bootenv
  when "local"
    mac_list.each_index do |idx|
      ["#{tftproot}/nodes/#{mac_list[idx].downcase}.grub",
       "#{tftproot}/nodes/#{mac_list[idx].upcase}.grub"].each do |grubfile|
        template grubfile do
          source "grub.local.erb"
          action "create"
        end
      end
    end
  when "sledgehammer"
    pxe_params = node["crowbar"]["provisioner"]["server"]["sledgehammer_kernel_params"].split(' ')
    pxe_params << "crowbar.fqdn=#{mnode_name}"
    provisioner_bootfile mnode_name do
      kernel_params pxe_params.join(" ")
      address mac_list
      bootenv "sledgehammer"
      action :add
    end
    template "#{node_dir}/#{mnode_name}/control.sh" do
      source "control.sh.erb"
      mode "0755"
      variables(:provisioner_name => node.name,
                :online => node["crowbar"]["provisioner"]["server"]["online"],
                :domain => domain_name,
                :provisioner_web => provisioner_web,
                :proxy => node["crowbar"]["provisioner"]["server"]["proxy"],
                :keys => (node["crowbar"]["provisioner"]["server"]["access_keys"] rescue Hash.new).values.sort.join($/),
                :v4_addr => node.address("admin",IP::IP4).addr
                )
    end
  when "ubuntu-12.04-install"
    provisioner_debian mnode_name do
      distro "ubuntu"
      version "12.04"
      address mac_list
      target mnode_name
      action :add
    end
  when "ubuntu-14.04-install"
    provisioner_debian mnode_name do
      distro "ubuntu"
      version "14.04"
      address mac_list
      target mnode_name
      action :add
    end
  when "centos-6.5-install"
    provisioner_redhat mnode_name do
      distro "centos"
      version "6.5"
      address mac_list
      target mnode_name
      action :add
    end
  when "centos-6.6-install"
    provisioner_redhat mnode_name do
      distro "centos"
      version "6.6"
      address mac_list
      target mnode_name
      action :add
    end
  when "redhat-6.5-install"
    provisioner_redhat mnode_name do
      distro "redhat"
      version "6.5"
      address mac_list
      target mnode_name
      action :add
    end
  when "fedora-20-install"
    provisioner_fedora mnode_name do
      distro "fedora"
      version "20"
      address mac_list
      target mnode_name
      action :add
    end
  when "redhat-7.0-install"
    provisioner_fedora mnode_name do
      distro "redhat"
      version "7.0"
      address mac_list
      target mnode_name
      action :add
    end
  when "centos-7.0.1406-install"
    provisioner_fedora mnode_name do
      distro "centos"
      version "7.0.1406"
      address mac_list
      target mnode_name
      action :add
    end
  else
    Chef::Log.info("Not messing with boot files for bootenv #{bootenv}")
  end
  mac_list.each_index do |idx|
    if bootenv == "local"
      dhcp_opts = []
    else
      dhcp_opts = [
"  if option arch = 00:07 or option arch = 00:09 {
      filename = \"grub-x86_64.efi\";
   } else {
      filename = \"grub.pxe\";
   }",
                   "next-server #{provisioner_addr}"]
    end
    dhcp_host "#{mnode_name}-#{idx}" do
      hostname mnode_name
      ipaddress v4addr.addr
      macaddress mac_list[idx]
      options dhcp_opts
      action :add
    end
  end
  file "#{node_dir}/#{mnode_name}/bootstate" do
    action :create
    content bootenv
  end
end

# Now that we have handled any updates we care about, delete any info about nodes we have deleted.
(node["crowbar_wall"]["dhcp"]["clients"].keys - new_clients.keys).each do |old_node_name|
  old_node = node["crowbar_wall"]["dhcp"]["clients"][old_node_name]
  mac_list = old_node["mac_addresses"] || []
  mac_list.each_index do |idx|
    a = dhcp_host "#{old_node_name}-#{idx}" do
      hostname old_node_name
      ipaddress "0.0.0.0"
      macaddress mac_list[idx]
      action :nothing
    end
    a.run_action(:remove)
  end
  a = provisioner_bootfile old_node_name do
    action :nothing
    address mac_list
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
  only_if "which selinuxenabled && selinxenabled"
end

node.normal["crowbar_wall"]["dhcp"]["clients"]=new_clients
