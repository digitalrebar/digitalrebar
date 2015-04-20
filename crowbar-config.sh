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
      "last": "223.255.255.255/0"
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
    "address": "192.168.124.1/24",
    "pref": "10"
  }
}'

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
crowbar roles bind dns-service to "system-phantom.internal.local"
crowbar roles bind ntp-service to "system-phantom.internal.local"
crowbar roles bind proxy-service to "system-phantom.internal.local"
crowbar roles bind dns-mgmt_service to "system-phantom.internal.local"
crowbar roles bind provisioner-service to "system-phantom.internal.local"
crowbar roles bind crowbar-api_service to "system-phantom.internal.local"
crowbar roles bind crowbar-job_runner_service to "system-phantom.internal.local"
crowbar roles bind crowbar-access to "system-phantom.internal.local"

# Set the domain name to use to the derived one
ROLE_ID=`crowbar roles show dns-service | grep '"id"'`
ROLE_ID=${ROLE_ID##*:}
ROLE_ID=${ROLE_ID%,}
NODE_ROLE_ID=`crowbar noderoles list | grep -B2 -A2 "\"role_id\":$ROLE_ID" | grep -B3 -A2 '"node_id": 1' | grep \"id\"`
NODE_ROLE_ID=${NODE_ROLE_ID##*:}
NODE_ROLE_ID=${NODE_ROLE_ID%,}
crowbar noderoles set $NODE_ROLE_ID attrib dns-domain to "{ \"value\": \"$DOMAINNAME\" }"

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

    ROLE_ID=`crowbar roles show crowbar-access | grep '"id"'`
    ROLE_ID=${ROLE_ID##*:}
    ROLE_ID=${ROLE_ID%,}
    NODE_ROLE_ID=`crowbar noderoles list | grep -B2 -A2 "\"role_id\":$ROLE_ID" | grep -B3 -A2 '"node_id": 1' | grep \"id\"`
    NODE_ROLE_ID=${NODE_ROLE_ID##*:}
    NODE_ROLE_ID=${NODE_ROLE_ID%,}

    crowbar noderoles set $NODE_ROLE_ID attrib crowbar-access_keys to "`cat /tmp/key_list`"

    crowbar noderoles set $NODE_ROLE_ID attrib crowbar-machine_key to "{ \"value\": \"`cat /etc/crowbar.install.key`\" }"

    rm -rf /tmp/key_list
fi

crowbar nodes commit "system-phantom.internal.local"

# Create the catch all network
crowbar networks create "$unmanaged_net"

## Create a stupid default admin network
crowbar networks create "$admin_net"

## Create the equally stupid BMC network
crowbar networks create "$bmc_net"

# Join the admin node into the rails app and make it manageable
./crowbar-node.sh 127.0.0.1

# Bind the admin role to it, and commit the resulting
# proposed noderoles.

crowbar roles bind crowbar-build-root-key to "$FQDN"

crowbar roles bind crowbar-api_server to "$FQDN"
crowbar roles bind crowbar-job_runner to "$FQDN"

# TODO: One day do it this way:
# Setup DNS Server and Mgmt Shim for our own DNS Server
# crowbar roles bind dns-server to "$FQDN"
# crowbar roles bind dns-mgmt_shim_crowbar_dns to "$FQDN"
crowbar roles bind dns-database to "$FQDN"

# Set the dns forwarder if you have them
DNS_FORWARDER=""
#DNS_FORWARDER="YOUR DNS IP HERE"
if [ "$DNS_FORWARDER" != "" ] ; then
    ROLE_ID=`crowbar roles show dns-server | grep '"id"'`
    ROLE_ID=${ROLE_ID##*:}
    ROLE_ID=${ROLE_ID%,}
    NODE_ROLE_ID=`crowbar noderoles list | grep -B2 -A2 "\"role_id\":$ROLE_ID" | grep -B3 -A2 '"node_id": 2' | grep \"id\"`
    NODE_ROLE_ID=${NODE_ROLE_ID##*:}
    NODE_ROLE_ID=${NODE_ROLE_ID%,}
    crowbar noderoles set $NODE_ROLE_ID attrib dns-forwarders to "{ \"value\": [ \"$DNS_FORWARDER\" ] }"
fi

# Example external dns server - use instead of dns-database above
#curl -X PUT -d '{"Datacenter": "dc1", "Node": "external", "Address": "209.18.47.61", "Service": {"Service": "dns-service", "Port": 43, "Tags": [ "system" ]} }' http://127.0.0.1:8500/v1/catalog/register

# Ntp Service configuration
# Use the admin node as the ntp server for the cluster
crowbar roles bind ntp-server to "$FQDN"

# Example external ntp server - use instead of ntp-server above
#curl -X PUT -d '{"Datacenter": "dc1", "Node": "external", "Address": "pool.ntp.org", "Service": {"Service": "ntp-service", "Port": 123, "Tags": [ "system" ]} }' http://127.0.0.1:8500/v1/catalog/register

# Proxy Service configuration
# Use the admin node as the proxy server for the cluster
crowbar roles bind proxy-server to "$FQDN"

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
crowbar roles bind dhcp-database to "$FQDN"

# Setup Up provisioner.
crowbar roles bind provisioner-server to "$FQDN"
crowbar roles bind provisioner-database to "$FQDN"
crowbar roles bind provisioner-repos to "$FQDN"
crowbar roles bind provisioner-docker-setup to "$FQDN"

# Add the now mostly empty admin-node
crowbar roles bind crowbar-admin-node to "$FQDN"

# Add the admin node to the admin network for now.
crowbar roles bind "network-$admin_net_name" to "$FQDN"

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
