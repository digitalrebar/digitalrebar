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
d_opts = node[:rebar][:dhcp][:options] || []

case node[:platform]
when "ubuntu","debian"
  case node[:lsb][:codename]
  when "maverick"
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
  else
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

domain_name = (node[:rebar][:dns][:domain] || node[:domain] rescue node[:domain])
lease_time = node[:rebar][:dhcp][:lease_time] || 60
Chef::Log.info("Webservers: #{ node['rebar'].inspect}")
next_server_ip = node['rebar']['provisioner']['server']['webservers'].first["address"]

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

# Get just the addresses
nameservers = node[:rebar][:dns][:nameservers].collect { |x| x['address'] }

# Build a list of local information
my_nics = ::Nic.nics
my_addresses = {}
my_nics.each do |nic|
  next if nic.loopback?
  nic.addresses.each do |ii|
    next unless ii.v4?
    my_addresses[ii] = nic
  end
end

found = []
node[:rebar][:dhcp][:networks].each do |name, net|
  router = net["router"]["address"]
  net_pools = net["ranges"].select{|range|["dhcp","host"].include? range["name"]}
  subnet = IP.coerce(net_pools[0]["first"])

  # Mark as found
  my_addresses.keys.each do |ii|
    found << ii if ii.network == subnet.network
  end

  dhcp_subnet subnet.network do
    action :add
    pools net_pools
    pool_options pool_opts
    router router
    options [ "option domain-name \"#{domain_name}\"",
              "option domain-name-servers #{nameservers.join(", ")}",
              "default-lease-time #{lease_time}",
              "ignore-client-uids true",
              "max-lease-time #{lease_time * 3}"]
  end
end

# Remove non-served local nets we have have added previously
found_nics = []
found.each do |ii|
  found_nics << my_addresses[ii]
  my_addresses.delete(ii)
end

# For all remaining addresses, check if the nic was use already
#   If so, then remove a network for this address
#   Otherwise, create a fake one to listen on.
my_addresses.keys.each do |ii|
  if found_nics.include? my_addresses[ii]
    dhcp_subnet ii.network do
      action :remove
    end
  else
    found_nics << my_addresses[ii]
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
    else
      service_name "isc-dhcp-server"
    end
  end
  supports :restart => true, :status => true, :reload => true
  action [ :enable, :start ]
end

script "restart consul" do
  interpreter "bash"
  action :nothing
  code "/usr/local/bin/consul reload"
end

file "/etc/consul.d/dhcp.json" do
  content <<EOC
{
  "service": {
    "name": "dhcp",
    "tags": [ "system" ],
    "check": {
      "script": "pgrep dhcpd",
      "interval": "10s"
    }
  }
}
EOC
  action :create
  notifies :run, "script[restart consul]"
end
