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

[[ $1 ]] || {
    echo "Must pass the FQDN you want the admin node to have as the first argument!"
    exit 1
}

FQDN=$1

DOMAINNAME=${FQDN#*.}
if [[ ! -f /.dockerenv ]]; then
    HOSTNAME=${FQDN%%.*}
    # Fix up the localhost address mapping.
    sed -i -e "s/\(127\.0\.0\.1.*\)/127.0.0.1 $FQDN $HOSTNAME localhost.localdomain localhost/" /etc/hosts
    sed -i -e "s/\(127\.0\.1\.1.*\)/127.0.1.1 $FQDN $HOSTNAME localhost.localdomain localhost/" /etc/hosts
    # Fix Ubuntu/Debian Hostname
    echo "$FQDN" > /etc/hostname
    hostname $FQDN
else
    HOSTNAME=$(cat /etc/hostname)
    FQDN="${HOSTNAME}.${DOMAINNAME}"
fi

export FQDN

# Fix CentOs/RedHat Hostname
if [ -f /etc/sysconfig/network ] ; then
  sed -i -e "s/HOSTNAME=.*/HOSTNAME=$FQDN/" /etc/sysconfig/network
fi

# Set domainname (for dns)
echo "$DOMAINNAME" > /etc/domainname

if [[ $http_proxy && !$upstream_proxy ]] && ! pidof squid; then
    export upstream_proxy=$http_proxy
fi

# Update the provisioner server template to use whatever
# proxy the admin node should be using.
if [[ $upstream_proxy ]]; then
    crowbar roles set provisioner-server \
        attrib provisioner-upstream_proxy \
        to "{\"value\": \"${upstream_proxy}\"}"
fi

crowbar roles set provisioner-os-install \
    attrib provisioner-target_os \
    to '{"value": "centos-7.0.1406"}'

set -e
set -x
admin_net='
{
  "name": "admin",
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

bmc_net='
{
  "name": "bmc",
  "deployment": "system",
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

# Create a stupid default admin network
crowbar networks create "$admin_net"

# Create the equally stupid BMC network
crowbar networks create "$bmc_net"

# Create the admin node entry.
crowbar nodes create "$admin_node"

# Bind the admin role to it, and commit the resulting
# proposed noderoles.
crowbar roles bind crowbar-admin-node to "$FQDN"
crowbar nodes commit "$FQDN"

# Figure out what IP addresses we should have, and add them.
netline=$(crowbar nodes addresses "$FQDN" on admin)
nets=(${netline//,/ })
for net in "${nets[@]}"; do
    [[ $net =~ $ip_re ]] || continue
    net=${BASH_REMATCH[1]}
    # Make this more complicated and exact later.
    ip addr add "$net" dev eth0 || :
    echo "${net%/*} $FQDN" >> /etc/hosts || :
done

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
