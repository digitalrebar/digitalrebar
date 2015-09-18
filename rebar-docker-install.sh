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

export PS4='${BASH_SOURCE}@${LINENO}(${FUNCNAME[0]}): '
set -x
date

. /etc/profile
cd /opt/digitalrebar/core

# Load the initial barclamp
echo "Loading the core barclamp metadata"
/opt/digitalrebar/core/bin/barclamp_import /opt/digitalrebar/core

# Load the rest of the barclamps
while read bc; do
  echo "Loading barclamp metadata from $bc"
  /opt/digitalrebar/core/bin/barclamp_import "$bc"
done < <(find /opt/digitalrebar -name rebar.yml |grep -v '/core/')

# We absolutely have to have an unmanaged-internal network, and there can
# really only be one of them, so leave this hardcoded for now.
unmanaged_net='
{
  "category": "unmanaged",
  "group": "internal",
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

# Create the catch all network
rebar networks show 'unmanaged-internal' >/dev/null 2>&1 || rebar networks import "$unmanaged_net"

DOMAINNAME=$BASE_DOMAINNAME
echo "{ \"domain\": \"$DOMAINNAME\" }" > config/domain.json

# This is a hack as well for now.
echo "{ \
  \"name\": \"default\", \
  \"priority\": 50, \
  \"template\": \"{{node.name}}.$DOMAINNAME\", \
  \"matcher\": \"net.category == \\\"admin\\\"\", \
  \"service\": \"system\" \
}" > config/filters/admin-default.json

cp /home/rebar/.ssh/id_rsa.pub config/ssh_keys/admin-0.key

./rebar-build-json.rb > config/final.json

curl -X PUT --data-binary @config/final.json http://127.0.0.1:8500/v1/kv/digitalrebar/private/bootstrap?token=$CONSUL_M_ACL

[[ $FQDN ]] || export FQDN="$(hostname)"

HOSTNAME=${FQDN%%.*}

# Get config file from consul
CONSUL_MACL=$CONSUL_M_ACL
curl "http://127.0.0.1:8500/v1/kv/digitalrebar/private/bootstrap?token=$CONSUL_MACL&raw" > config/processed.json

# Process networks
admin_nets=()
network_count=`jq ".networks | length" config/processed.json`
for ((i=0; i < network_count; i++)) ; do
  network=`jq ".networks[$i]" config/processed.json`
  group=`jq -r ".networks[$i].group" config/processed.json`
  category=`jq -r ".networks[$i].category" config/processed.json`
  if ! [[ $category && $group ]]; then
      echo "Network must have a category and a group defined!"
      exit 1
  fi
  name="$category-$group"
  if rebar networks show $name >/dev/null 2>&1 ; then
    rebar networks update $name "$network"
  else
    rebar networks import "$network"
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
  rebar networks add $network to "$FQDN"

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
  rebar nodes bind "system-phantom.internal.local" to "$service_role"
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
    rebar nodes bind "$FQDN" to $k
  done

  attrs=`jq ".services[$i].attributes" config/processed.json`
  if [ "$attrs" != "null" ] ; then
    count=`jq ".services[$i].attributes|keys|length" config/processed.json`
    for ((k=0; k < count; k++)) ; do
      kname=`jq -r ".services[$i].attributes|keys|.[$k]" config/processed.json`
      kvalue=`jq ".services[$i].attributes[\"$kname\"]" config/processed.json`

      rebar nodes set "$FQDN" attrib $kname to "{ \"value\": $kvalue }"
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
    datacenter="digitalrebar"
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

# GREG: External service should already exist
#  curl -X PUT -d "{\"Datacenter\": \"$datacenter\", \"Node\": \"$node_name\", \"Address\": \"$service_ip\", \"Service\": {\"Service\": \"$service_name\", \"Port\": $service_port, \"Address\": \"$service_ip\", \"Tags\": [ \"$service_tag\" ]} }" http://127.0.0.1:8500/v1/catalog/register

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
  if rebar deployments show $name >/dev/null 2>&1 ; then
    rebar deployments update $name "$deployment"
  else
    rebar deployments create "$deployment"
  fi

  # Add roles
  dr_count=`jq ".deployments[$i].roles | length" config/processed.json`
  for ((dri=0; dri < dr_count; dri++)) ; do
    dr_role=`jq -r ".deployments[$i].roles[$dri]" config/processed.json`

    rebar deployments bind $name to $dr_role 2>/dev/null || true
  done

  # Update attributes
  count=`jq ".deployments[$i].attributes|keys|length" config/processed.json`
  for ((k=0; k < count; k++)) ; do
    kname=`jq -r ".deployments[$i].attributes|keys|.[$k]" config/processed.json`
    kvalue=`jq ".deployments[$i].attributes[\"$kname\"]" config/processed.json`

    rebar deployments set $name attrib $kname to "{ \"value\": $kvalue }"
  done

  rebar deployments commit $name
done

# Commit the phantom
rebar nodes propose "system-phantom.internal.local"
rebar nodes set "system-phantom.internal.local" attrib dns-domain to "{ \"value\": \"$DOMAINNAME\" }"
rebar nodes commit "system-phantom.internal.local"

# Add the now mostly empty admin-node
# GREG: rebar nodes bind "$FQDN" to rebar-build-root-key
# GREG: rebar nodes bind "$FQDN" to rebar-admin-node

# Add keys into the system
rebar deployments bind system to rebar-access
keys=`jq -r .ssh_keys config/processed.json`
rebar deployments set system attrib rebar-access_keys to "{ \"value\": $keys }"
rebar deployments set system attrib rebar-machine_key to "{ \"value\": \"`cat /etc/rebar.install.key`\" }"

rebar deployments commit system

# Add/Update DNS Filters into the system
filter_count=`jq ".filters | length" config/processed.json`
for ((i=0; i < filter_count; i++)) ; do
  dnf=`jq ".filters[$i]" config/processed.json`
  name=`jq -r ".filters[$i].name" config/processed.json`
  if rebar dnsnamefilters show $name >/dev/null 2>&1 ; then
    rebar dnsnamefilters update $name "$dnf"
  else
    rebar dnsnamefilters create "$dnf"
  fi
done

# Add/Update users into the system
user_count=`jq ".users | length" config/processed.json`
for ((i=0; i < user_count; i++)) ; do
  user=`jq ".users[$i]" config/processed.json`
  name=`jq -r ".users[$i].username" config/processed.json`
  if rebar users show $name >/dev/null 2>&1 ; then
    rebar users update $name "$user"
  else
    rebar users import "$user"
  fi
done

# Mark the node as alive.
echo "Configuration Complete, you can watch annealing from the UI.  \`su - rebar\` to begin managing the system."
# Converge the admin node.
rebar converge && date

