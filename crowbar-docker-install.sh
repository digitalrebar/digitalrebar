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

set -x
date

# Add /opt/chef/bin to path for systems that don't have chef-client "normally"
cat > /etc/profile.d/chef-path.sh <<EOF
export PATH="\$PATH:/opt/chef/bin"
EOF

. /etc/profile
cd /opt/opencrowbar/core

# Wait for the crowbar server to startup
sleep 20 # GREG:

if [[ ! -e /etc/crowbar.install.key ]]; then
  key=`dd if=/dev/urandom bs=64 count=1 2>/dev/null | sha512sum - 2>/dev/null | awk '{ print $1 }'`
  echo "Creating machine-install user"
  machine_user="
{
  \"username\": \"machine-install\",
  \"email\": \"root@localhost.localdomain\",
  \"password\": \"$key\",
  \"password_confirmation\": \"$key\",
  \"remember_me\": false,
  \"is_admin\": false,
  \"digest\": true
}"

  if ! crowbar -U crowbar -P crowbar users import "$machine_user"; then
    echo "Could not create machine-install user!"
    exit 1
  fi
  echo "machine-install:$key" >/etc/crowbar.install.key
  cat >/etc/profile.d/crowbar-key.sh <<EOF
export CROWBAR_KEY=machine-install:$key
EOF
fi

# Load the initial barclamp
echo "Loading the core barclamp metadata"
/opt/opencrowbar/core/bin/barclampe_import /opt/opencrowbar/core

# Load the rest of the barclamps
while read bc; do
  echo "Loading barclamp metadata from $bc"
  /opt/opencrowbar/core/bin/barclampe_import "$bc"
done < <(find /opt/opencrowbar -name crowbar.yml |grep -v '/core/')

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

# Create the catch all network
crowbar networks show 'unmananged' >/dev/null 2>&1 && crowbar networks import "$unmanaged_net"

# Build a map of keys in the /root/.ssh/authorized_keys
# Record the machine key as well. -- THIS IS NOT GREAT
#
# This is after the join to make sure that the crowbar-access
# role is added to the system deployment.
#
if [ -e /root/.ssh/authorized_keys ] ; then
    count=1
    rm -f /tmp/key_list
    echo "{ \"value\": {" > /tmp/key_list
    COMMA=""
    cat /root/.ssh/authorized_keys | while read line; do
        echo "$line" > config/ssh_keys/"admin-$count.key"
        echo "$COMMA \"admin-$count\": \"$line\"" >> /tmp/key_list
        count=`expr $count + 1`
        COMMA=","
    done
    echo "} }" >> /tmp/key_list
    crowbar deployments bind system to crowbar-access >/dev/null 2>&1 || true
    crowbar deployments set system attrib crowbar-access_keys to "`cat /tmp/key_list`"
    crowbar deployments set system attrib crowbar-machine_key to "{ \"value\": \"`cat /etc/crowbar.install.key`\" }"
    crowbar deployments commit system
    rm -rf /tmp/key_list
fi

# This may not be needed
echo "{ \"domain\": \"$DOMAINNAME\" }" > config/domain.json

# This is a hack as well for now.
echo "{ \
  \"name\": \"default\", \
  \"priority\": 50, \
  \"template\": \"{{node.name}}.$DOMAINNAME\", \
  \"matcher\": \"net.category == \\\"admin\\\"\", \
  \"service\": \"system\" \
}" > config/filters/admin-default.json

./crowbar-build-json.rb > config/final.json

curl -X PUT --data-binary @config/final.json http://127.0.0.1:8500/v1/kv/opencrowbar/private/bootstrap?token=$CONSUL_M_ACL

[[ $FQDN ]] || export FQDN="$(hostname)"

DOMAINNAME="neode.com"
HOSTNAME=${FQDN%%.*}

# Get config file from consul
CONSUL_MACL=$CONSUL_M_ACL
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
# GREG: crowbar nodes set "system-phantom.internal.local" attrib dns-domain to "{ \"value\": \"$DOMAINNAME\" }"
crowbar nodes commit "system-phantom.internal.local"

# Add the now mostly empty admin-node
# GREG: crowbar nodes bind "$FQDN" to crowbar-build-root-key
# GREG: crowbar nodes bind "$FQDN" to crowbar-admin-node

# Add keys into the system
crowbar deployments bind system to crowbar-access
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
  if crowbar users show $name >/dev/null 2>&1 ; then
    crowbar users update $name "$user"
  else
    crowbar users import "$user"
  fi
done

# Mark the node as alive.
echo "Configuration Complete, you can watch annealing from the UI.  \`su - crowbar\` to begin managing the system."
# Converge the admin node.
crowbar converge && date

