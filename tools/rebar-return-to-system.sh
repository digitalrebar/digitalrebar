#!/usr/bin/env bash

#
# tools/rebar-return-to-system.sh <node id or name>
#
# Clears the non-system noderoles and places the node in system
# After rebooting it.
#

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

# Get system deployment id.
SYS_ID=$(rebar deployments show system | jq .id)

#
# Remove all the node roles in the current deployment (if not system)
#
current_deployment=$(rebar nodes show $node_id | jq -r .deployment_id)
if [[ $SYS_ID != $current_deployment ]] ; then
    ids=$(rebar noderoles match "{ \"node_id\": $node_id, \"deployment_id\": $current_deployment }" | jq -r .[].id)

    if [[ $ids != '' ]] ; then
        for i in $ids ; do
	    rebar noderoles destroy $i
	done
    fi
fi

# Move the node to the new env
rebar nodes move $node_id to $SYS_ID

# Mark the node into sledgehammer and reboot
rebar nodes update $node_id '{ "bootenv": "discovery" }'
rebar nodes power $node_id reboot

