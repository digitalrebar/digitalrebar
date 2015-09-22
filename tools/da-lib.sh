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

bridge="docker0"
bridge_re='-b=([^ ])'
bridge_addr_re='inet ([0-9.]+)/'
# If we told Docker to use a custom bridge, here is where it is at.
[[ $(ps -C docker -o 'command=') =~ $bridge_re ]] && \
    bridge="${BASH_REMATCH[1]}"
if ! [[ -f $mountdir/core/config/networks/the_admin.json ]]; then
    echo "Cannot find the_admin.json to get the address we should add to the Docker bridge!"
    exit 1
fi
bridge_router_ip="$(jq -r '.router.address' < "$mountdir/core/config/networks/the_admin.json")"

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
    
    if ! fgrep -q "$bridge_router_ip" < <(ip addr show dev "$bridge"); then
        sudo ip addr add "$bridge_router_ip" dev "$bridge"
    fi
    
    sudo rm -rf "$HOME/.cache/digitalrebar/tftpboot/nodes"
    if [[ -f $HOME/.ssh/id_rsa.pub ]]; then
        cp "$HOME/.ssh/id_rsa.pub" "$mountdir/core/config/ssh_keys/$USER-0.key"
    fi
    
    cd "$mountdir/deploy/compose"
    if (which selinuxenabled && which chcon) &>/dev/null && selinuxenabled; then
        sudo chcon -Rt svirt_sandbox_file_t "$mountdir"
        sudo chcon -Rt svirt_sandbox_file_t "$HOME/.cache/digitalrebar/tftpboot"
    fi
    
    docker-compose pull
    docker-compose up -d
}


tear_down_admin_containers() {
    cd "$mountdir/deploy/compose"
    docker-compose kill
    docker-compose rm -f
    sudo rm -rf "$mountdir/deploy/compose/data-dir"
    sudo ip addr del "$bridge_router_ip" dev "$bridge" || :
}
