#!/bin/bash

# Setup database tasks
. /opt/digitalrebar/core/setup/00-rebar-rake-tasks.install

# Start up the code
. /opt/digitalrebar/core/setup/01-rebar-start.install

# Build initial access keys
. /opt/digitalrebar/core/setup/02-make-machine-key.install

[[ $REBAR_ENDPOINT ]] || export REBAR_ENDPOINT="https://${IP}:3000"

make_service rebar-api 3000 '{"script": "rebar -E $REBAR_ENDPOINT -U rebar -P rebar1 ping","interval": "10s"}'
consul reload

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
  /opt/digitalrebar/core/bin/barclamp_import "$bc" || :
done < <(find /opt/digitalrebar -name rebar.yml |grep -v '/core/')

# Create the system deployment
# We always need a system deployment
if ! rebar deployments show system; then
    rebar deployments create '{"name": "system", 
                               "description": "Created Automatically by System",
                               "system": true}' && \
        rebar deployments commit system || exit 1
fi

DOMAINNAME=$BASE_DOMAINNAME

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

# Add keys into the system
bind_service rebar-api_service
rebar deployments bind system to rebar-access || :
keys=`jq -r .ssh_keys ${BUILT_CFG_FILE}`
set_service_attrib rebar-access rebar-access_keys "{ \"value\": $keys }"
set_service_attrib rebar-access rebar-machine_key "{ \"value\": \"`cat /etc/rebar.install.key`\" }"

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
