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

# THIS IS A HACK FOR ANSIBLE
#
# Ansible in docker has issues with ssh parms
# Mostly having to do with directory mapping.
#
sed -i '/\[ssh_connection\]/a ssh_args=' /etc/ansible/ansible.cfg
# END HACK

BUILT_CFG_FILE=/tmp/final.json
export REBAR_KEY="$(cat /etc/rebar.install.key)"

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

# Create the system deployment
# We always need a system deployment
if ! rebar deployments create '{"name": "system", "description": "Created Automatically by System","system": true}'; then cat /var/log/rebar/production.log; exit 1; fi

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

./rebar-build-json.rb > ${BUILT_CFG_FILE}

# Process networks
admin_nets=()
network_count=`jq ".networks | length" ${BUILT_CFG_FILE}`
for ((i=0; i < network_count; i++)) ; do
  network=`jq ".networks[$i]" ${BUILT_CFG_FILE}`
  group=`jq -r ".networks[$i].group" ${BUILT_CFG_FILE}`
  category=`jq -r ".networks[$i].category" ${BUILT_CFG_FILE}`
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

# Process services/servers
service_count=`jq ".services | length" ${BUILT_CFG_FILE}`
for ((i=0; i < service_count; i++)) ; do
  service_type=`jq -r ".services[$i].type" ${BUILT_CFG_FILE}`
  service_name=`jq -r ".services[$i].name" ${BUILT_CFG_FILE}`

  has_service=`jq -r ".services[$i].has_service" ${BUILT_CFG_FILE}`
  if [ "$has_service" == "false" ] ; then
    continue
  fi

  service_role=`jq -r ".services[$i].service_role" ${BUILT_CFG_FILE}`
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
  service_type=`jq -r ".services[$i].type" ${BUILT_CFG_FILE}`
  service_name=`jq -r ".services[$i].name" ${BUILT_CFG_FILE}`

  if [ "$service_type" == "external" ] ; then
    continue
  fi

  server_role=`jq -r ".services[$i].server_role" ${BUILT_CFG_FILE}`
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

  attrs=`jq ".services[$i].attributes" ${BUILT_CFG_FILE}`
  if [ "$attrs" != "null" ] ; then
    count=`jq ".services[$i].attributes|keys|length" ${BUILT_CFG_FILE}`
    for ((k=0; k < count; k++)) ; do
      kname=`jq -r ".services[$i].attributes|keys|.[$k]" ${BUILT_CFG_FILE}`
      kvalue=`jq ".services[$i].attributes[\"$kname\"]" ${BUILT_CFG_FILE}`

      rebar nodes set "$FQDN" attrib $kname to "{ \"value\": $kvalue }"
    done
  fi
done
for ((i=0; i < service_count; i++)) ; do
  service_type=`jq -r ".services[$i].type" ${BUILT_CFG_FILE}`
  service_name=`jq -r ".services[$i].name" ${BUILT_CFG_FILE}`

  if [ "$service_type" == "internal" ] ; then
    continue
  fi

  if [[ $service_name == *"-"* ]] ; then
    service_name="${service_name}_service"
  else
    service_name="${service_name}-service"
  fi

  datacenter=`jq -r ".services[$i].datacenter" ${BUILT_CFG_FILE}`
  if [ "$datacenter" == "null" ] ; then
    datacenter="digitalrebar"
  fi
  node_name=`jq -r ".services[$i].node_name" ${BUILT_CFG_FILE}`
  if [ "$node_name" == "null" ] ; then
    node_name="External"
  fi

  service_ip=`jq -r ".services[$i].service_ip" ${BUILT_CFG_FILE}`
  service_port=`jq -r ".services[$i].service_port" ${BUILT_CFG_FILE}`
  service_tag=`jq -r ".services[$i].service_tag" ${BUILT_CFG_FILE}`
  if [ "$service_tag" == "null" ] ; then
    service_tag="system"
  fi

# GREG: External service should already exist
#  curl -X PUT -d "{\"Datacenter\": \"$datacenter\", \"Node\": \"$node_name\", \"Address\": \"$service_ip\", \"Service\": {\"Service\": \"$service_name\", \"Port\": $service_port, \"Address\": \"$service_ip\", \"Tags\": [ \"$service_tag\" ]} }" http://127.0.0.1:8500/v1/catalog/register

  attrs=`jq ".services[$i].keys" ${BUILT_CFG_FILE}`
  if [ "$attrs" != "null" ] ; then
    CONSUL_MACL=$(jq .acl_master_token </etc/consul.d/default.json | awk -F\" '{ print $2 }')

    count=`jq ".services[$i].keys|keys|length" ${BUILT_CFG_FILE}`
    for ((k=0; k < count; k++)) ; do
      kname=`jq -r ".services[$i].keys|keys|.[$k]" ${BUILT_CFG_FILE}`
      kvalue=`jq -r ".services[$i].keys[\"$kname\"]" ${BUILT_CFG_FILE}`

      curl -X PUT -d "$kvalue" http://127.0.0.1:8500/v1/kv/$kname?token=$CONSUL_MACL
    done
  fi
done

# Deployments
deployment_count=`jq ".deployments | length" ${BUILT_CFG_FILE}`
for ((i=0; i < deployment_count; i++)) ; do
  name=`jq -r ".deployments[$i].deployment.name" ${BUILT_CFG_FILE}`
  deployment=`jq ".deployments[$i].deployment" ${BUILT_CFG_FILE}`

  # Create or update the deployment
  if rebar deployments show $name >/dev/null 2>&1 ; then
    rebar deployments update $name "$deployment"
  else
    rebar deployments create "$deployment"
  fi

  # Add roles
  dr_count=`jq ".deployments[$i].roles | length" ${BUILT_CFG_FILE}`
  for ((dri=0; dri < dr_count; dri++)) ; do
    dr_role=`jq -r ".deployments[$i].roles[$dri]" ${BUILT_CFG_FILE}`

    rebar deployments bind $name to $dr_role 2>/dev/null || true
  done

  # Update attributes
  count=`jq ".deployments[$i].attributes|keys|length" ${BUILT_CFG_FILE}`
  for ((k=0; k < count; k++)) ; do
    kname=`jq -r ".deployments[$i].attributes|keys|.[$k]" ${BUILT_CFG_FILE}`
    kvalue=`jq ".deployments[$i].attributes[\"$kname\"]" ${BUILT_CFG_FILE}`

    rebar deployments set $name attrib $kname to "{ \"value\": $kvalue }"
  done

  rebar deployments commit $name
done

# Commit the phantom
rebar nodes propose "system-phantom.internal.local"
rebar nodes set "system-phantom.internal.local" attrib dns-domain to "{ \"value\": \"$DOMAINNAME\" }"
rebar nodes commit "system-phantom.internal.local"

# Add keys into the system
rebar deployments bind system to rebar-access
keys=`jq -r .ssh_keys ${BUILT_CFG_FILE}`
rebar deployments set system attrib rebar-access_keys to "{ \"value\": $keys }"
rebar deployments set system attrib rebar-machine_key to "{ \"value\": \"`cat /etc/rebar.install.key`\" }"

rebar deployments commit system

printf 'export REBAR_KEY="%s"\n' "$REBAR_KEY" >/etc/profile.d/rebar-key.sh

# Add/Update DNS Filters into the system
filter_count=`jq ".filters | length" ${BUILT_CFG_FILE}`
for ((i=0; i < filter_count; i++)) ; do
  dnf=`jq ".filters[$i]" ${BUILT_CFG_FILE}`
  name=`jq -r ".filters[$i].name" ${BUILT_CFG_FILE}`
  if rebar dnsnamefilters show $name >/dev/null 2>&1 ; then
    rebar dnsnamefilters update $name "$dnf"
  else
    rebar dnsnamefilters create "$dnf"
  fi
done

# Add/Update users into the system
user_count=`jq ".users | length" ${BUILT_CFG_FILE}`
for ((i=0; i < user_count; i++)) ; do
  user=`jq ".users[$i]" ${BUILT_CFG_FILE}`
  name=`jq -r ".users[$i].username" ${BUILT_CFG_FILE}`
  if rebar users show $name >/dev/null 2>&1 ; then
    rebar users update $name "$user"
  else
    rebar users import "$user"
  fi
done
