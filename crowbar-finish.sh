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
    crowbar networks import "$network"
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


# Process services/servers
service_count=`jq ".services | length" config/processed.json`
for ((i=0; i < service_count; i++)) ; do
  service_type=`jq -r ".services[$i].type" config/processed.json`
  service_name=`jq -r ".services[$i].name" config/processed.json`

  has_service=`jq -r ".services[$i].has_service" config/processed.json`
  if [ "$has_service" == "false" ] ; then
    continue
  fi

  service_role=`jq -r ".services[$i].service_role" config/processed.json`
  if [ "$service_role" == "null" ] ; then
    if [[ $service_name == *"-"* ]] ; then
      service_role="${service_name}_service"
    else
      service_role="${service_name}-service"
    fi
  fi

  # Add the internal services
  crowbar nodes bind "system-phantom.internal.local" to "$service_role"
done
for ((i=0; i < service_count; i++)) ; do
  # Add external servers.
  service_type=`jq -r ".services[$i].type" config/processed.json`
  service_name=`jq -r ".services[$i].name" config/processed.json`

  if [ "$service_type" == "external" ] ; then
    continue
  fi

  server_role=`jq -r ".services[$i].server_role" config/processed.json`
  if [ "$server_role" == "null" ] ; then
    if [[ $service_name == *"-"* ]] ; then
      server_role="${service_name}_server"
    else
      server_role="${service_name}-server"
    fi
  fi

  IFS=',' read -ra ADDR <<< "$server_role"
  for k in "${ADDR[@]}"; do
    crowbar nodes bind "$FQDN" to $k
  done

  attrs=`jq ".services[$i].attributes" config/processed.json`
  if [ "$attrs" != "null" ] ; then
    count=`jq ".services[$i].attributes|keys|length" config/processed.json`
    for ((k=0; k < count; k++)) ; do
      kname=`jq -r ".services[$i].attributes|keys|.[$k]" config/processed.json`
      kvalue=`jq ".services[$i].attributes[\"$kname\"]" config/processed.json`

      crowbar nodes set "$FQDN" attrib $kname to "{ \"value\": $kvalue }"
    done
  fi
done
for ((i=0; i < service_count; i++)) ; do
  service_type=`jq -r ".services[$i].type" config/processed.json`
  service_name=`jq -r ".services[$i].name" config/processed.json`

  if [ "$service_type" == "internal" ] ; then
    continue
  fi

  if [[ $service_name == *"-"* ]] ; then
    service_name="${service_name}_service"
  else
    service_name="${service_name}-service"
  fi

  datacenter=`jq -r ".services[$i].datacenter" config/processed.json`
  if [ "$datacenter" == "null" ] ; then
    datacenter="opencrowbar"
  fi
  node_name=`jq -r ".services[$i].node_name" config/processed.json`
  if [ "$node_name" == "null" ] ; then
    node_name="External"
  fi

  service_ip=`jq -r ".services[$i].service_ip" config/processed.json`
  service_port=`jq -r ".services[$i].service_port" config/processed.json`
  service_tag=`jq -r ".services[$i].service_tag" config/processed.json`
  if [ "$service_tag" == "null" ] ; then
    service_tag="system"
  fi

  curl -X PUT -d "{\"Datacenter\": \"$datacenter\", \"Node\": \"$node_name\", \"Address\": \"$service_ip\", \"Service\": {\"Service\": \"$service_name\", \"Port\": $service_port, \"Address\": \"$service_ip\", \"Tags\": [ \"$service_tag\" ]} }" http://127.0.0.1:8500/v1/catalog/register

  attrs=`jq ".services[$i].keys" config/processed.json`
  if [ "$attrs" != "null" ] ; then
    CONSUL_MACL=$(jq .acl_master_token </etc/consul.d/default.json | awk -F\" '{ print $2 }')

    count=`jq ".services[$i].keys|keys|length" config/processed.json`
    for ((k=0; k < count; k++)) ; do
      kname=`jq -r ".services[$i].keys|keys|.[$k]" config/processed.json`
      kvalue=`jq -r ".services[$i].keys[\"$kname\"]" config/processed.json`

      curl -X PUT -d "$kvalue" http://127.0.0.1:8500/v1/kv/$kname?token=$CONSUL_MACL
    done
  fi
done

# Deployments
deployment_count=`jq ".deployments | length" config/processed.json`
for ((i=0; i < deployment_count; i++)) ; do
  name=`jq -r ".deployments[$i].deployment.name" config/processed.json`
  deployment=`jq ".deployments[$i].deployment" config/processed.json`

  # Create or update the deployment
  if crowbar deployments show $name >/dev/null 2>&1 ; then
    crowbar deployments update $name "$deployment"
  else
    crowbar deployments create "$deployment"
  fi

  # Add roles
  dr_count=`jq ".deployments[$i].roles | length" config/processed.json`
  for ((dri=0; dri < dr_count; dri++)) ; do
    dr_role=`jq -r ".deployments[$i].roles[$dri]" config/processed.json`

    crowbar deployments bind $name to $dr_role 2>/dev/null || true
  done

  # Update attributes
  count=`jq ".deployments[$i].attributes|keys|length" config/processed.json`
  for ((k=0; k < count; k++)) ; do
    kname=`jq -r ".deployments[$i].attributes|keys|.[$k]" config/processed.json`
    kvalue=`jq ".deployments[$i].attributes[\"$kname\"]" config/processed.json`

    crowbar deployments set $name attrib $kname to "{ \"value\": $kvalue }"
  done

  crowbar deployments commit $name
done

# Commit the phantom
crowbar nodes propose "system-phantom.internal.local"
crowbar nodes set "system-phantom.internal.local" attrib dns-domain to "{ \"value\": \"$DOMAINNAME\" }"
crowbar nodes commit "system-phantom.internal.local"

# Add the now mostly empty admin-node
crowbar nodes bind "$FQDN" to crowbar-build-root-key
crowbar nodes bind "$FQDN" to crowbar-admin-node

# Add keys into the system
keys=`jq -r .ssh_keys config/processed.json`
crowbar deployments set system attrib crowbar-access_keys to "{ \"value\": $keys }"
crowbar deployments commit system

# Add/Update DNS Filters into the system
filter_count=`jq ".filters | length" config/processed.json`
for ((i=0; i < filter_count; i++)) ; do
  dnf=`jq ".filters[$i]" config/processed.json`
  name=`jq -r ".filters[$i].name" config/processed.json`
  if crowbar dnsnamefilters show $name >/dev/null 2>&1 ; then
    crowbar dnsnamefilters update $name "$dnf"
  else
    crowbar dnsnamefilters create "$dnf"
  fi
done

# Add/Update users into the system
user_count=`jq ".users | length" config/processed.json`
for ((i=0; i < user_count; i++)) ; do
  user=`jq ".users[$i]" config/processed.json`
  name=`jq -r ".users[$i].username" config/processed.json`
  crowbar users import "$user"
done

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
    while ! crowbar users list; do
        sleep 1
    done
fi

crowbar nodes commit "$FQDN"

# Mark the node as alive.
crowbar nodes update "$FQDN" '{"alive": true}'
echo "Configuration Complete, you can watch annealing from the UI.  \`su - crowbar\` to begin managing the system."
# Converge the admin node.
crowbar converge && date

