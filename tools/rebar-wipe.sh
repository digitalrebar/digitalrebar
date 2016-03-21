#!/usr/bin/env bash

#
# tools/rebar-wipe.sh <node id or name>
#
# Wipes a node using the RackN burnin feature.
#

WIPE_DEPLOYMENT='cleaning'

. workloads/wl-init.sh

# Add args here

. workloads/wl-lib.sh

NODE_ID=$1
shift

if [[ $NODE_ID == '' ]] ; then
    echo "Must specify a node identifier"
    exit 1
fi

if ! rebar nodes show $NODE_ID 2>/dev/null >/dev/null ; then
    echo "Must specify valid node"
    exit 1
fi

node_id=$(rebar nodes show $NODE_ID | jq -r .id)

system_node=$(rebar nodes show $node_id | jq -r .system)
if [[ $system_node == true ]] ; then
    echo "Don't wipe system nodes"
    exit 1
fi

# Build the wipe deployment if missing.
if ! rebar deployments show $WIPE_DEPLOYMENT 2>/dev/null >/dev/null ;
then
    rebar deployments create "{ \"name\": \"$WIPE_DEPLOYMENT\", \"description\": \"RackN Decommision Deployment\" }" 2>/dev/null >/dev/null
    if [[ $? -ne 0 ]] ; then
        echo "Failed to create deployment: $WIPE_DEPLOYMENT"
        exit 1
    fi
fi

# Get system deployment id.
SYS_ID=$(rebar deployments show system | jq .id)

#
# Determine of there is a raid-configure role on the node.
# if so, add it back and a jbod configuration in the new deployment.
#
raid_disk_count=$(rebar nodes get $node_id attrib raid-detected-controllers | jq -r '[.value[].disks|length] | add')

#
# Remove all the node roles in the current deployment (if not system)
#
current_deployment=$(rebar nodes show $node_id | jq -r .deployment_id)
if [[ $SYS_ID != $current_deployment ]] ; then
    ids=$(rebar noderoles match "{ \"node_id\": $node_id, \"deployment_id\": $current_deployment }" | jq -r .[].id)

    if [[ $ids != '' ]] ; then
        for i in $ids ; do
            if rebar noderoles show $i 2>/dev/null >/dev/null ; then
                rebar noderoles destroy $i
            fi
	done
    fi
fi

# Move the node to the new env
rebar nodes move $node_id to $WIPE_DEPLOYMENT

# Add the decommision role
rebar nodes bind $node_id to decommission

# Add the raid-configure role back.
if [[ $raid_disk_count != null ]] ; then
    rebar nodes bind $node_id to raid-post-configure

    echo '{ "value": [' > /tmp/disk.$$
    comma=""
    for ((i=0;i<$raid_disk_count;i++)); do
      echo "$comma{ \"boot\": false, \"disks\": 1, \"name\": \"disk$i\", \"raid_level\": \"jbod\", \"size\": \"min\" }" >> /tmp/disk.$$
      comma=","
    done
    echo '] }' >> /tmp/disk.$$

    rebar nodes set $node_id attrib raid-wanted-volumes to "$(cat /tmp/disk.$$)"
    rm -f /tmp/disk.$$
fi

# Mark the node into sledgehammer and reboot
rebar nodes update $node_id '{ "bootenv": "sledgehammer" }'
rebar nodes power $node_id reboot

# Commit the deployment
rebar deployments commit $WIPE_DEPLOYMENT

echo "Once decommision role is complete, run: tools/rebar-return-to-system.sh --admin-ip=$ADMIN_IP $NODE_ID"

