#!/bin/bash

set -e

startdir="$PWD"

if [[ $0 = /* ]]; then
    mountdir="$0"
elif [[ $0 = .*  || $0 = */* ]]; then
    mountdir="$(readlink -f "$PWD/$0")"
else
    echo "Cannot figure out where core is!"
    exit 1
fi

mountdir="${mountdir%/*}"

# Walk up the directory tree, and stop when we find $mountdir/core/tools/docker-admin-up

while [[ ! -x $mountdir/core/tools/docker-admin-up ]]; do
    mountdir="${mountdir%/*}"
    if [[ ! $mountdir ]]; then
        echo "Failed to find core repository!"
        exit 1
    fi
done

. "$mountdir/core/tools/da-lib.sh"
cleaned=false
cleanup() {
    res=$?
    set +e
    pkill kvm-slave
    mkdir -p "$startdir/rebar"
    (cd "$mountdir/deploy/compose"; docker-compose logs > "$startdir/rebar/compose.log")
    tear_down_admin_containers
    exit $res
}

die() {
    printf '%s\n' "$@"
    exit 1
}

trap cleanup 0 INT QUIT TERM

bring_up_admin_containers && wait_for_admin_containers || \
        die "Failed to deploy admin node"

# No party mix until I figure out what is happening with weird ioctls.
exit 0

PARTY_MIX=(centos-6.6 centos-7.1.1503 debian-7.8.0 debian-8.1.0
           fedora-20 redhat-6.5 ubuntu-12.04 ubuntu-14.04
           ubuntu-15.04)

START_NODES="$(rebar nodes list |jq 'length')"

ADDED_NODES=${#PARTY_MIX}

for ((n=0; n<$ADDED_NODES; n++)); do
    "$mountdir/core/tools/kvm-slave" &
    sleep 15
done

WANTED_NODES=$((START_NODES + ADDED_NODES))

while (( WANTED_NODES > $(rebar nodes list |jq 'length') )); do
    sleep 60
done

rebar converge

nodes=( $(rebar nodes list |jq -r 'map(select(.["bootenv"] == "sledgehammer")) | .[] | .["name"]') )

for i in "${!PARTY_MIX[@]}"; do
    rebar nodes bind "${nodes[i]}" to rebar-installed-node
    rebar nodes set "${nodes[i]}" attrib provisioner-target_os to "{\"value\": \"${PARTY_MIX[i]}\"}"
    rebar nodes commit "${nodes[i]}"
done

sleep 10
rebar converge
