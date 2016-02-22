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
    echo "Killing KVM slaves"
    pkill kvm-slave
    rm -rf "$startdir/rebar" || :
    mkdir -p "$startdir/rebar"
    echo "Killing admin containers"
    (
        cd "$mountdir/deploy/compose"
        docker-compose kill
        for container in $(docker-compose ps | awk '/^compose/ {print $1}'); do
            mkdir -p "$startdir/rebar/$container"
            docker logs "$container" &> "$startdir/rebar/$container.log"
            docker cp "$container:/var/log" "$startdir/rebar/$container"
        done
        docker-compose rm -f
    ) &>/dev/null
    exit $res
}

die() {
    printf '%s\n' "$@"
    exit 1
}

trap cleanup 0 INT QUIT TERM

docker_admin_default_containers

bring_up_admin_containers && wait_for_admin_containers || \
        die "Failed to deploy admin node"

[[ $DEPLOY_OSES ]] || exit 0

PARTY_MIX=(centos-6.6 centos-7.2.1511 debian-7 debian-8
           redhat-6.5 ubuntu-12.04 ubuntu-14.04 ubuntu-15.04)
printf "Bringing up slaves for party mix testing: "
for ((n=0; n<${#PARTY_MIX[@]}; n++)); do
    printf "."
    "$mountdir/core/tools/kvm-slave" &>/dev/null &
done
echo " Done"

matchstr='{"alive": true,
           "available": true,
           "admin": false,
           "system": false,
           "bootenv": "sledgehammer"}'
nodes=()
printf "Waiting for nodes to become alive: "
while (( ${#nodes[@]} != ${#PARTY_MIX[@]} )); do
    sleep 10
    printf "."
    nodes=($(rebar nodes match "$matchstr" | jq -r '.[] |.["name"]') )
done
echo " Done"

converge || exit 1

for i in "${!PARTY_MIX[@]}"; do
    echo "Setting ${nodes[i]} to install ${PARTY_MIX[i]}"
    (
        rebar nodes bind "${nodes[i]}" to rebar-installed-node
        rebar nodes set "${nodes[i]}" attrib provisioner-target_os to "{\"value\": \"${PARTY_MIX[i]}\"}"
        rebar nodes commit "${nodes[i]}"
    ) >&/dev/null
done

echo "Waiting on OS installation to finish"
converge || exit 1
echo "Finished."
