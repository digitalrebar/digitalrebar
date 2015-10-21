#!/bin/bash

die() {
    printf '%s\n' "$@"
    exit 1
}

export PS4='${BASH_SOURCE}@${LINENO}(${FUNCNAME[0]}): '
if ! which docker &>/dev/null; then
    die "Please install Docker!"
fi

if (which selinuxenabled && selinuxenabled) &>/dev/null; then
    die "SELinux is enabled!  It does not play nice with some of our containers, please disable it."
fi

if ! which docker-compose &>/dev/null; then
    die "Please install docker-compose!"
fi

# Make sure that the tftp conntrack modules are loaded
sudo modprobe nf_conntrack_tftp
sudo modprobe nf_nat_tftp

rebar() {
    docker exec compose_rebar_api_1 rebar -E http://127.0.0.1:3000 -U rebar -P rebar1 "$@"
}

retry_until() {
    # $1 = seconds to wait
    # $2 = message to print on timeout
    # rest = command to retry
    local count="$1" msg="$2"
    shift 2
    while ! "$@" &>/dev/null; do
        count=$((count - 1))
        if ((count == 0)); then
            echo "$msg" >&2
            return 1
        fi
        sleep 1
    done
}

bridge="docker0"
bridge_re='-b=([^ ])'
bridge_addr_re='inet ([0-9.]+)/'
# If we told Docker to use a custom bridge, here is where it is at.
[[ $(ps -C docker -o 'command=') =~ $bridge_re ]] && bridge="${BASH_REMATCH[1]}"


bring_up_admin_containers() {
    # Clone the deploy repo to a known location, if we don't already have it.
    if [[ ! -d $mountdir/deploy ]]; then
        # Clone the deploy repo to a known location.
        git clone https://github.com/rackn/digitalrebar-deploy "$mountdir/deploy"
    elif [[ ! -d $mountdir/deploy/compose ]]; then
        echo "$mountdir/deploy does not contain a checkout of the digitalrebar container deployer!"
    elif [[ ! -L  $mountdir/deploy/compose/digitalrebar ]]; then
        ln -s "$mountdir" \
           "$mountdir/deploy/compose/digitalrebar"
    fi
    
    mkdir -p "$HOME/.cache/digitalrebar/tftpboot"
    
    sudo rm -rf "$HOME/.cache/digitalrebar/tftpboot/nodes"
    if [[ -f $HOME/.ssh/id_rsa.pub ]]; then
        cp "$HOME/.ssh/id_rsa.pub" "$mountdir/core/config/ssh_keys/$USER-0.key"
    fi
    
    cd "$mountdir/deploy/compose"
    if (which selinuxenabled && which chcon) &>/dev/null && selinuxenabled; then
        sudo chcon -Rt svirt_sandbox_file_t "$mountdir"
        sudo chcon -Rt svirt_sandbox_file_t "$HOME/.cache/digitalrebar/tftpboot"
    fi
    
    if [ "$NO_PULL" == "" ] ; then
      docker-compose $COMPOSE_ARGS pull
    fi
    docker-compose $COMPOSE_ARGS up -d
}

wait_for_admin_containers() {
    echo "Waiting on API to start (up to 240 seconds)"
    retry_until 240 \
                "Took too long for rebar container to come up" \
                rebar ping || exit 1
    echo "Waiting for the provisioner (up to 480 seconds)"
    retry_until 480 \
                "Took too long for the provisioner to come up" \
                rebar nodes show provisioner.local.neode.org || exit 1
    sleep 5
    echo "Waiting for rebar to converge (up to 10 minutes)"
    if ! rebar converge; then
        echo "Rebar failed to converge!"
        exit 1
    fi
}

tear_down_admin_containers() {
    cd "$mountdir/deploy/compose"
    docker-compose $COMPOSE_ARGS kill
    docker-compose $COMPOSE_ARGS rm -f
    sudo rm -rf "$mountdir/deploy/compose/data-dir"
}
