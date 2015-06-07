#!/bin/bash
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

set -e
date

. /etc/profile
cd /opt/opencrowbar/core
. ./bootstrap.sh

check_hostname

[[ $FQDN ]] || export FQDN="$(hostname)"

DOMAINNAME=${FQDN#*.}
HOSTNAME=${FQDN%%.*}

if [[ $http_proxy && !$upstream_proxy ]] && ! pidof squid; then
    export upstream_proxy=$http_proxy
fi

# Add consul to the default deployment, and make sure it uses the same
# acl master token and encryption key as the current running consul.
crowbar deployments bind system to consul

for k in acl_master_token encrypt datacenter domain acl_datacenter \
                          acl_default_policy acl_down_policy; do
    v="$(jq ".${k}" </etc/consul.d/default.json)"
    [[ $v = null ]] && continue
    crowbar deployments set system attrib "consul-${k//_/-}" to "{\"value\": $v}"
done

crowbar deployments commit system

# Update the provisioner server template to use whatever
# proxy the admin node should be using.
if [[ $upstream_proxy ]]; then
    crowbar roles set proxy-server \
        attrib proxy-upstream_proxy \
        to "{\"value\": \"${upstream_proxy}\"}"
fi

crowbar roles set provisioner-os-install \
    attrib provisioner-target_os \
    to '{"value": "centos-7.1.1503"}'

set -e
set -x

unmanaged_net='
{
  "name": "unmanaged",
  "category": "unmanaged",
  "deployment": "system",
  "conduit": "?1g0",
  "configure": false,
  "ranges": [
    {
      "overlap": true,
      "name": "host",
      "first": "0.0.0.1/0",
      "last": "223.255.255.254/0"
    },
    {
      "overlap": true,
      "name": "host-v6",
      "first": "::1/0",
      "last": "ffff:ffff:ffff:ffff:ffff:ffff:ffff:fffe/0"
    }
  ]
}'

admin_net_name='the_admin'
admin_net='
{
  "name": "the_admin",
  "category": "admin",
  "group": "internal",
  "deployment": "system",
  "conduit": "1g0",
  "ranges": [
    {
      "name": "admin",
      "first": "192.168.124.10/24",
      "last": "192.168.124.11/24"
    },
    {
      "name": "host",
      "first": "192.168.124.81/24",
      "last": "192.168.124.254/24"
    },
    {
      "name": "dhcp",
      "first": "192.168.124.21/24",
      "last": "192.168.124.80/24"
    }
  ],
  "router": {
    "address": "192.168.124.10/24",
    "pref": "10"
  }
}'

bmc_net_name='the_bmc'
bmc_net='
{
  "name": "the_bmc",
  "deployment": "system",
  "category": "bmc",
  "group": "internal",
  "conduit": "bmc",
  "ranges": [
    {
      "conduit": "1g0",
      "name": "admin",
      "first": "192.168.128.10/24",
      "last": "192.168.128.20/24"
    },
    {
      "name": "host",
      "first": "192.168.128.21/24",
      "last": "192.168.128.254/24"
    }
  ],
  "router": {
    "address": "192.168.128.10/24",
    "pref": "99"
  }
}'


admin_node="
{
  \"name\": \"$FQDN\",
  \"admin\": true,
  \"alive\": false,
  \"bootenv\": \"local\"
}
"

###
# This should vanish once we have a real bootstrapping story.
###
ip_re='([0-9a-f.:]+/[0-9]+)'

# Add required or desired services
services=(proxy-service dns-service ntp-service dns-mgmt_service provisioner-service
          crowbar-api_service crowbar-job_runner_service amqp-service)
for role in "${services[@]}"; do
    crowbar nodes bind "system-phantom.internal.local" to "$role"
done
crowbar nodes set "system-phantom.internal.local" attrib dns-domain to "{ \"value\": \"$DOMAINNAME\" }"

crowbar nodes commit "system-phantom.internal.local"

# Create the catch all network
crowbar networks create "$unmanaged_net"

## Create a stupid default admin network
crowbar networks create "$admin_net"

## Create the equally stupid BMC network
crowbar networks create "$bmc_net"

# Join the admin node into the rails app and make it manageable
./crowbar-node.sh 127.0.0.1

# Build a map of keys in the /root/.ssh/authorized_keys
# Record the machine key as well. -- THIS IS NOT GREAT
if [ -e /root/.ssh/authorized_keys ] ; then
    count=1
    rm -f /tmp/key_list
    echo "{ \"value\": {" > /tmp/key_list
    COMMA=""
    cat /root/.ssh/authorized_keys | while read line; do
        echo "$COMMA \"admin-$count\": \"$line\"" >> /tmp/key_list
        count=`expr $count + 1`
        COMMA=","
    done
    echo "} }" >> /tmp/key_list
    crowbar deployments set system attrib crowbar-access_keys to "`cat /tmp/key_list`"
    crowbar deployments set system attrib crowbar-machine_key to "{ \"value\": \"`cat /etc/crowbar.install.key`\" }"
    crowbar deployments commit system
    rm -rf /tmp/key_list
fi

# Add the admin node to the admin network for now.
crowbar networks add $admin_net_name to "$FQDN"

# Add the admin node to the bmc network for now.
crowbar networks add $bmc_net_name to "$FQDN"

# Bind the admin role to it, and commit the resulting
# proposed noderoles.

crowbar nodes bind "$FQDN" to crowbar-build-root-key

crowbar nodes bind "$FQDN" to crowbar-api_server
crowbar nodes bind "$FQDN" to crowbar-job_runner

crowbar nodes bind "$FQDN" to rabbitmq-server

# TODO: One day do it this way:
# Setup DNS Server and Mgmt Shim for our own DNS Server
# crowbar nodes bind "$FQDN" to dns-server
# crowbar nodes bind "$FQDN" to dns-mgmt_shim_crowbar_dns
crowbar nodes bind "$FQDN" to dns-database

# Set the dns forwarder if you have them
DNS_FORWARDER=""
#DNS_FORWARDER="YOUR DNS IP HERE"
if [ "$DNS_FORWARDER" != "" ] ; then
    crowbar nodes set "$FQDN" attrib dns-forwarders to "{ \"value\": [ \"$DNS_FORWARDER\" ] }"
fi

# Example external dns server - use instead of dns-database above
#curl -X PUT -d '{"Datacenter": "dc1", "Node": "external", "Address": "209.18.47.61", "Service": {"Service": "dns-service", "Port": 43, "Tags": [ "system" ]} }' http://127.0.0.1:8500/v1/catalog/register

# Ntp Service configuration
# Use the admin node as the ntp server for the cluster
crowbar nodes bind "$FQDN" to ntp-server

# Example external ntp server - use instead of ntp-server above
#curl -X PUT -d '{"Datacenter": "dc1", "Node": "external", "Address": "pool.ntp.org", "Service": {"Service": "ntp-service", "Port": 123, "Tags": [ "system" ]} }' http://127.0.0.1:8500/v1/catalog/register

# Proxy Service configuration
# Use the admin node as the proxy server for the cluster
crowbar nodes bind "$FQDN" to proxy-server

# Example external proxy server - use instead of proxy-server above
#curl -X PUT -d '{"Datacenter": "dc1", "Node": "external", "Address": "fred.clam.shack.com", "Service": {"Service": "proxy-service", "Port": 8123, "Tags": [ "system" ]} }' http://127.0.0.1:8500/v1/catalog/register

# Setup DHCP Server - this can be optional
#
# If not used, the external DHCP server will need
# to use the ADMIN NODE IP as the next server
# and serve grub or pxelinux image.
#
# Here is an ISC DHCP Server stanza to use for an external dhcp server.
# ++++++++
# subnet 192.168.124.0 netmask 255.255.255.0 {
#   option routers 192.168.124.10;  # Router here
#   option subnet-mask 255.255.255.0;
#   option broadcast-address 192.168.124.255;
#   option domain-name "neode.com";
#   option domain-name-servers 192.168.124.10; # Admin IP/DNS Server IP
#   default-lease-time 7200;
#   max-lease-time 36000;
#    pool {
#      range 192.168.124.81 192.168.124.254;   # Host Range from your Admin Network.
#      allow unknown-clients;
#      if option arch = 00:07 or option arch = 00:09 {
#        filename = "grub-x86_64.efi";
#      } else {
#        filename = "grub.pxe";
#      }
#      next-server 192.168.124.10;            # Admin IP/Provisioner IP
#    }
# }
# ++++++++
#
# Comment out this line if using your own DHCP server
crowbar nodes bind "$FQDN" to dhcp-database

# Setup Up provisioner.
crowbar nodes bind "$FQDN" to provisioner-server
crowbar nodes bind "$FQDN" to provisioner-database
crowbar nodes bind "$FQDN" to provisioner-repos
crowbar nodes bind "$FQDN" to provisioner-docker-setup

# Add the now mostly empty admin-node
crowbar nodes bind "$FQDN" to crowbar-admin-node

# Figure out what IP addresses we should have, and add them.
# If the above adds an address, we need to make sure it starts on the node.
netline=$(crowbar nodes addresses "$FQDN" on $admin_net_name)
nets=(${netline//,/ })
for net in "${nets[@]}"; do
    [[ $net =~ $ip_re ]] || continue
    net=${BASH_REMATCH[1]}
    # Make this more complicated and exact later.
    ip addr add "$net" dev eth0 || :
    echo "${net%/*} $FQDN" >> /etc/hosts || :
done

# If we have an http_proxy set, then we should make sure
# we have no-proxy setup correctly and use it.
if [[ $http_proxy ]] ; then
    # Now that we have shiny new IP addresses, make sure that Squid has the right
    # addresses in place for always_direct exceptions, and pick up the new proxy
    # environment variables.
    (
        . bootstrap.sh
        chef-solo -c /opt/opencrowbar/core/bootstrap/chef-solo.rb -o "${proxy_recipes}"
    )
    . /etc/profile

    # Make sure that Crowbar is running with the proper environment variables
    service crowbar stop
    service crowbar start
    while ! /opt/opencrowbar/core/bin/crowbar -U crowbar -P crowbar users list; do
        sleep 1
    done
fi

crowbar nodes commit "$FQDN"

# flag allows you to stop before final step
if ! [[ $* = *--zombie* ]]; then

  # Mark the node as alive.
  crowbar nodes update "$FQDN" '{"alive": true}'
  #curl -s -f --digest -u $(cat /etc/crowbar.install.key) \
  #    -X PUT "http://localhost:3000/api/v2/nodes/$FQDN" \
  #    -d 'alive=true'
  echo "Configuration Complete, you can watch annealing from the UI.  \`su - crowbar\` to begin managing the system."
  # Converge the admin node.
  crowbar converge && date
else
  echo "To complete configuration, mark node alive using: crowbar nodes update 1 '{""alive"": true}'"
fi
