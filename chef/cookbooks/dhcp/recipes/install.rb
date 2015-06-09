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

# Install the existing package to setup stuff for CentOS
# And paths and files and init scripts.
#
# The compile process will probably only work for CentOS

case node[:platform]
when "ubuntu","debian"
  package "dhcp3-server"
when "redhat","centos"
  package "dhcp"
when "suse"
  package "dhcp-server"
end

remote_file '/root/dhcp-4.3.2.tar.gz' do
  source 'http://opencrowbar.s3-website-us-east-1.amazonaws.com/dhcp-4.3.2.tar.gz'
end

bash 'expand DHCP source' do
  cwd '/root'
  code <<EOF
tar -zxvf /root/dhcp-4.3.2.tar.gz
EOF
end

cookbook_file '/root/dhcp-4.3.2/common/discover.c' do
  source 'discover.c'
end

cookbook_file '/root/dhcp-4.3.2/includes/dhcpd.h' do
  source 'dhcpd.h'
end

bash 'build DHCP server' do
  cwd '/root/dhcp-4.3.2'
  code <<EOF
./configure \
    --enable-dhcpv6 \
    --with-srv-lease-file=/var/lib/dhcpd/dhcpd.leases \
    --with-srv6-lease-file=/var/lib/dhcpd/dhcpd6.leases \
    --with-cli-lease-file=/var/lib/dhclient/dhclient.leases \
    --with-cli6-lease-file=/var/lib/dhclient/dhclient6.leases \
    --with-srv-pid-file=/var/run/dhcpd.pid \
    --with-srv6-pid-file=/var/run/dhcpd6.pid \
    --with-cli-pid-file=/var/run/dhclient.pid \
    --with-cli6-pid-file=/var/run/dhclient6.pid \
    --with-relay-pid-file=/var/run/dhcrelay.pid \
    --with-relay6-pid-file=/var/run/dhcrelay6.pid \
    --enable-paranoia
make
cp /usr/sbin/dhcpd /usr/sbin/dhcpd.orig
cp server/dhcpd /usr/sbin/dhcpd
EOF
  not_if { File.exists?('/usr/sbin/dhcpd.orig') }
end

