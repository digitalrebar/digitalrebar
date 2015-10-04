#!/bin/bash

export PS4='${BASH_SOURCE}@${LINENO}(${FUNCNAME[0]}): '
if ! which docker &>/dev/null; then
    echo "Please install Docker!"
    exit 1
fi

if (which selinuxenabled && selinuxenabled) &>/dev/null; then
    echo "SELinux is enabled!  It does not play nice with some of our containers, please disable it."
    exit 1
fi

if ! which docker-compose &>/dev/null; then
    echo "Please install docker-compose!"
    exit 1
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
      docker-compose pull
    fi
    docker-compose up -d
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
    docker-compose kill
    docker-compose rm -f
    sudo rm -rf "$mountdir/deploy/compose/data-dir"
}
