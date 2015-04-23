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

intfs = [node.interface.name]
d_opts = node[:crowbar][:dhcp][:options]

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
# XXX: This should be done by attribute injection - fix as part of provisioner service work
admin_ip = node.address("admin",IP::IP4)
admin_net = node[:crowbar][:network][:admin]
lease_time = node[:crowbar][:dhcp][:lease_time]
net_pools = admin_net["ranges"].select{|range|["dhcp","host"].include? range["name"]}

pool_opts = {
  "dhcp" => ['allow unknown-clients',
             '      if option arch = 00:06 {
      filename = "discovery/bootia32.efi";
   } else if option arch = 00:07 {
      filename = "discovery/bootx64.efi";
   } else {
      filename = "discovery/pxelinux.0";
   }',
             "next-server #{admin_ip.addr}" ],
  "host" => ['deny unknown-clients']
}

nameservers=node[:crowbar][:dns][:nameservers]

dhcp_subnet IP.coerce(net_pools[0]["first"]).network do
  action :add
  network admin_net
  pools net_pools
  pool_options pool_opts
  admin_ip admin_ip
  options [ "option domain-name \"#{domain_name}\"",
            "option domain-name-servers #{nameservers.join(", ")}",
            "default-lease-time #{lease_time}",
            "max-lease-time #{lease_time * 3}"]
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
