#!/bin/bash
# Copyright 2015, Greg Althaus
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

hostname_re='([[:alnum:]]+\.){2,}[[:alnum:]]+'

check_hostname() {
    [[ $(hostname) =~ $hostname_re ]] && return
    echo "The hostname for the system must already be set to its proper name!"
    exit 1
}

check_hostname

[[ $FQDN ]] || export FQDN="$(hostname)"

DOMAINNAME=${FQDN#*.}
HOSTNAME=${FQDN%%.*}

set -x

# Get config file from consul
CONSUL_MACL=$(jq .acl_master_token </etc/consul.d/default.json | awk -F\" '{ print $2 }')
curl "http://127.0.0.1:8500/v1/kv/opencrowbar/private/bootstrap?token=$CONSUL_MACL&raw" > config/processed.json

# Process networks
admin_nets=()
network_count=`jq ".networks | length" config/processed.json`
for ((i=0; i < network_count; i++)) ; do
  network=`jq ".networks[$i]" config/processed.json`
  name=`jq -r ".networks[$i].name" config/processed.json`
  category=`jq -r ".networks[$i].category" config/processed.json`
  if crowbar networks show $name >/dev/null 2>&1 ; then
    crowbar networks update $name "$network"
  else
    crowbar networks create "$network"
  fi
  if [ "$category" == "admin" ] ; then
    admin_nets=(${admin_nets[@]} $name)
  fi
done

function contains () {
  local arr=(${@:2})
  local el=$1
  local marr=(${arr[@]/#$el/})
  [[ "${#arr[@]}" != "${#marr[@]}" ]]
}

# join networks
network_count=`jq ".networks_to_join | length" config/processed.json`
for ((i=0; i < network_count; i++)) ; do
  network=`jq -r ".networks_to_join[$i]" config/processed.json`
  crowbar networks add $network to "$FQDN"

  if contains $network $admin_nets ; then
    admin_net_name=$network
  fi
done

# Add required services
services=(provisioner-service crowbar-api_service crowbar-job_runner_service)
for role in "${services[@]}"; do
    crowbar nodes bind "system-phantom.internal.local" to "$role"
done
crowbar nodes commit "system-phantom.internal.local"

# Bind the admin role to it, and commit the resulting
# proposed noderoles.
crowbar nodes bind "$FQDN" to crowbar-build-root-key
crowbar nodes bind "$FQDN" to crowbar-api_server
crowbar nodes bind "$FQDN" to crowbar-job_runner

# Setup Up provisioner.
crowbar nodes bind "$FQDN" to provisioner-server
crowbar nodes bind "$FQDN" to provisioner-database
crowbar nodes bind "$FQDN" to provisioner-repos
crowbar nodes bind "$FQDN" to provisioner-docker-setup

# Process services/servers
service_count=`jq ".services | length" config/processed.json`
for ((i=0; i < service_count; i++)) ; do
  service=`jq ".services[$i]" config/processed.json`
# GREG: Do someting with $service
done

# Add keys into the system
keys=`jq -r .ssh_keys config/processed.json`
crowbar deployments set system attrib crowbar-access_keys to "{ \"value\": $keys }"

# Add/Update DNS Filters into the system
filter_count=`jq ".filters | length" config/processed.json`
for ((i=0; i < filter_count; i++)) ; do
  user=`jq ".filters[$i]" config/processed.json`
  name=`jq -r ".filters[$i].name" config/processed.json`
  if crowbar dnsnamefilters show $name >/dev/null 2>&1 ; then
    crowbar dnsnamefilters update $name "$user"
  else
    crowbar dnsnamefilters create "$user"
  fi
done

# Add/Update users into the system
user_count=`jq ".users | length" config/processed.json`
for ((i=0; i < user_count; i++)) ; do
  user=`jq ".users[$i]" config/processed.json`
  name=`jq -r ".users[$i].name" config/processed.json`
  if crowbar users show $name >/dev/null 2>&1 ; then
    crowbar users update $name "$user"
  else
    crowbar users create "$user"
  fi
done

# Add the now mostly empty admin-node
crowbar nodes bind "$FQDN" to crowbar-admin-node

# GREG: Fix this
#crowbar nodes set "system-phantom.internal.local" attrib dns-domain to "{ \"value\": \"$DOMAINNAME\" }"

# Figure out what IP addresses we should have, and add them.
# If the above adds an address, we need to make sure it starts on the node.
ip_re='([0-9a-f.:]+/[0-9]+)'
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

# Mark the node as alive.
crowbar nodes update "$FQDN" '{"alive": true}'
echo "Configuration Complete, you can watch annealing from the UI.  \`su - crowbar\` to begin managing the system."
# Converge the admin node.
crowbar converge && date

