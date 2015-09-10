#!/bin/bash

. core/tests/prequel.sh
launch_admin && run_admin || die "Failed to deploy admin node"

# No party mix until I figure out what is happening with weird ioctls.
exit 0

PARTY_MIX=(centos-6.6 centos-7.1.1503 debian-7.8.0 debian-8.1.0
           fedora-20 redhat-6.5 ubuntu-12.04 ubuntu-14.04
           ubuntu-15.04)

rebar deployments create '{"name": "default"}'

START_NODES="$(rebar nodes list |jq 'length')"

ADDED_NODES=${#PARTY_MIX}

for ((n=0; n<$ADDED_NODES; n++)); do
    core/tools/kvm-slave &
    sleep 15
done

WANTED_NODES=$((START_NODES + ADDED_NODES))

while (( WANTED_NODES > $(rebar nodes list |jq 'length') )); do
    sleep 60
done

rebar converge

nodes=( $(rebar nodes list |jq -r 'map(select(.["system"] != true and .["admin"] != true)) | .[] | .["name"]') )

for i in "${!PARTY_MIX[@]}"; do
    rebar nodes move "${nodes[i]}" to default
    rebar roles bind rebar-installed-node to "${nodes[i]}"
    rebar nodes set "${nodes[i]}" attrib provisioner-target_os to "{\"value\": \"${PARTY_MIX[i]}\"}"
done

rebar deployments commit default
sleep 10
rebar converge
