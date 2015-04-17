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

include_recipe "utils"
include_recipe "bind9::install"

directory "/etc/dhcp3"
directory "/etc/dhcp3/groups.d"
directory "/etc/dhcp3/subnets.d"
directory "/etc/dhcp3/hosts.d"

file "/etc/dhcp3/groups.d/group_list.conf" do
  owner "root"
  group "root"
  mode 0644
end
file "/etc/dhcp3/subnets.d/subnet_list.conf" do
  owner "root"
  group "root"
  mode 0644
end
file "/etc/dhcp3/hosts.d/host_list.conf" do
  owner "root"
  group "root"
  mode 0644
end

bash "build omapi key" do
  code <<-EOH
    cd /etc/dhcp3
    dnssec-keygen -r /dev/urandom  -a HMAC-MD5 -b 512 -n HOST omapi_key
    KEY=`cat /etc/dhcp3/Komapi_key*.private|grep ^Key|cut -d ' ' -f2-`
    echo $KEY > /etc/dhcp3/omapi.key
EOH
  not_if "test -f /etc/dhcp3/omapi.key"
end

intfs = [] # [node.interface.name]
d_opts = node[:crowbar][:dhcp][:options] || []

case node[:platform]
when "ubuntu","debian"
  case node[:lsb][:codename]
  when "natty","oneiric","precise"
    template "/etc/dhcp/dhcpd.conf" do
      owner "root"
      group "root"
      mode 0644
      source "dhcpd.conf.erb"
      variables(:options => d_opts)
      notifies :restart, "service[dhcp3-server]"
    end
    template "/etc/default/isc-dhcp-server" do
      owner "root"
      group "root"
      mode 0644
      source "dhcp3-server.erb"
      variables(:interfaces => intfs)
      notifies :restart, "service[dhcp3-server]"
    end
  else
    template "/etc/dhcp3/dhcpd.conf" do
      owner "root"
      group "root"
      mode 0644
      source "dhcpd.conf.erb"
      variables(:options => d_opts)
      notifies :restart, "service[dhcp3-server]"
    end
    template "/etc/default/dhcp3-server" do
      owner "root"
      group "root"
      mode 0644
      source "dhcp3-server.erb"
      variables(:interfaces => intfs)
      notifies :restart, "service[dhcp3-server]"
    end
  end
when "redhat","centos"

  dhcp_config_file = case
    when node[:platform_version].to_f >= 6
      "/etc/dhcp/dhcpd.conf"
    else
      "/etc/dhcpd.conf"
    end

  template dhcp_config_file do
    owner "root"
    group "root"
    mode 0644
    source "dhcpd.conf.erb"
    variables(:options => d_opts)
    notifies :restart, "service[dhcp3-server]"
  end

  template "/etc/sysconfig/dhcpd" do
    owner "root"
    group "root"
    mode 0644
    source "redhat-sysconfig-dhcpd.erb"
    variables(:interfaces => intfs)
    notifies :restart, "service[dhcp3-server]"
  end

when "suse"
  template "/etc/dhcpd.conf" do
    owner "root"
    group "root"
    mode 0644
    source "dhcpd.conf.erb"
    variables(:options => d_opts)
    notifies :restart, "service[dhcp3-server]"
  end

  template "/etc/sysconfig/dhcpd" do
    owner "root"
    group "root"
    mode 0644
    source "suse-sysconfig-dhcpd.erb"
    variables(:interfaces => intfs)
    notifies :restart, "service[dhcp3-server]"
  end
end

domain_name = (node[:crowbar][:dns][:domain] || node[:domain] rescue node[:domain])
lease_time = node[:crowbar][:dhcp][:lease_time] || 60
next_server_ip = node['crowbar']['provisioner']['server']['webservers'].first.match(/^(.*):.*$/).captures.first

pool_opts = {
  "dhcp" => ['allow unknown-clients',
             '      if option arch = 00:06 {
      filename = "discovery/bootia32.efi";
   } else if option arch = 00:07 {
      filename = "discovery/bootx64.efi";
   } else {
      filename = "discovery/pxelinux.0";
   }',
             "next-server #{next_server_ip}" ],
  "host" => ['deny unknown-clients']
}

nameserver = node[:crowbar][:dns][:nameservers]

# Build a list of local information
my_nics = ::Nic.nics
my_addresses = []
my_nics.each do |nic|
  next if nic.loopback?
  nic.addresses.each do |ii|
    next unless ii.v4?
    my_addresses << ii
  end
end

found = false
node[:crowbar][:dhcp][:networks].each do |name, net|
  router = net["router"]["address"]
  net_pools = net["ranges"].select{|range|["dhcp","host"].include? range["name"]}
  subnet = IP.coerce(net_pools[0]["first"])

  # Remove the local address if in a subnet
  remove_me = nil
  my_addresses.each do |ii|
    remove_me = ii if ii.network == subnet.network
  end
  if remove_me
    my_addresses.delete(remove_me)
    found = true
  end

  dhcp_subnet subnet.network do
    action :add
    pools net_pools
    pool_options pool_opts
    router router
    options [ "option domain-name \"#{domain_name}\"",
              "option domain-name-servers #{nameservers.join(", ")}",
              "default-lease-time #{lease_time}",
              "max-lease-time #{lease_time * 3}"]
  end
end

if found
  # Remove non-served local nets we have have added previously
  my_addresses.each do |ii|
    dhcp_subnet ii.network do
      action :remove
    end
  end
else
  # if no network is local to the dhcp server, we need to add one.
  my_addresses.each do |ii|
    dhcp_subnet ii.network do
      action :add
      pools []
      pool_options {}
      router nil
      options []
    end
  end
end

service "dhcp3-server" do
  case node[:platform]
  when "redhat", "centos", "suse"
    service_name "dhcpd" 
  when "ubuntu"
    case node[:lsb][:codename]
    when "maverick"
      service_name "dhcp3-server"
    when "natty", "oneiric", "precise"
      service_name "isc-dhcp-server"
    end
  end
  supports :restart => true, :status => true, :reload => true
  action [ :enable, :start ]
end
