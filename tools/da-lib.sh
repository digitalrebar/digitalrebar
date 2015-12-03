#!/bin/env bash

die() {
    printf '%s\n' "$@"
    exit 1
}

if [ "${BASH_VERSINFO}" -lt 4 ] ; then
    die "Must have a bash version of 4 or higher"
fi

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

rebar() {
    docker exec compose_rebar_api_1 rebar -E https://127.0.0.1:3000 -U rebar -P rebar1 "$@"
}

known_containers=(provisioner logging debug node access)

declare -A containers

use_container() {
    ! [[ ! ${containers[$1]} || ${containers[$1]} == false ]]
}

docker_admin_default_containers() {
    [[ ${containers["provisioner"]} ]] || containers["provisioner"]=true
    [[ ${containers["access"]} ]] || containers["access"]=FORWARDER
}

args=()

while (( $# > 0 )); do
    arg="$1"
    case $arg in
        --no-pull) export NO_PULL="Y";;
        --access)
            case $2 in
                HOST|FORWARDER)
                    containers["access"]="$2"
                    shift;;
                *) containers["access"]="FORWARDER";;
                esac;;
        --*)
            a="${arg#--}"
            is_set=false
            for cval in "${known_containers[@]}"; do
                if [[ $a = $cval ]]; then
                    containers["$cval"]=true
                    is_set=true
                elif [[ $a = no-$cval ]]; then
                    containers["$cval"]=false
                    is_set=true
                fi
                [[ $is_set = true ]] && break
            done
            if [[ $is_set = false ]]; then
                args+=("$arg")
            fi;;
        *) args+=("$arg");;
    esac
    shift
done
set -- "${args[@]}"

make_compose_args() {
    REAL_COMPOSE_ARGS="$COMPOSE_ARGS"
    for c in "${!containers[@]}"; do
        [[ ${containers["$c"]} && ${containers["$c"]} != false ]] || continue
        REAL_COMPOSE_ARGS="$REAL_COMPOSE_ARGS --$c"
        if [[ ${containers["$c"]} != true ]]; then
            REAL_COMPOSE_ARGS="$REAL_COMPOSE_ARGS ${containers[$c]}"
        fi
    done
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
        sudo rm -rf "$mountdir/deploy/compose/digitalrebar"
        ln -s "$mountdir" \
           "$mountdir/deploy/compose/digitalrebar"
    fi

    mkdir -p "$HOME/.cache/digitalrebar/tftpboot"

    sudo rm -rf "$HOME/.cache/digitalrebar/tftpboot/nodes"
    if [[ -f $HOME/.ssh/id_rsa.pub ]]; then
        cp "$HOME/.ssh/id_rsa.pub" "$mountdir/deploy/compose/config-dir/api/config/ssh_keys/$USER-0.key"
    fi

    cd "$mountdir/deploy/compose"
    if (which selinuxenabled && which chcon) &>/dev/null && selinuxenabled; then
        sudo chcon -Rt svirt_sandbox_file_t "$mountdir"
        sudo chcon -Rt svirt_sandbox_file_t "$HOME/.cache/digitalrebar/tftpboot"
    fi

    [[ -d $mountdir/deploy/compose/data-dir ]] && sudo rm -rf "$mountdir/deploy/compose/data-dir"
    ./init_files.sh --clean
    make_compose_args
    ./init_files.sh $REAL_COMPOSE_ARGS

    if [ "$NO_PULL" == "" ] ; then
        docker-compose pull
    fi
    if [[ ${containers["access"]} = FORWARDER && $(uname -s) != Darwin ]]; then
        sudo modprobe nf_nat_tftp
    fi
    docker-compose up -d
}

provisioner_finished() {
    rebar nodes match \
          '{"name": "provisioner.local.neode.org",
            "alive": true, "available": true}'  | \
        grep -F -q provisioner.local.neode.org
}

wait_for_admin_containers() {
    echo "Waiting on API to start (up to 240 seconds)"
    retry_until 240 \
                "Took too long for rebar container to come up" \
                rebar ping || exit 1
    if use_container provisioner; then
        echo "Waiting for the provisioner (up to 600 seconds)"
        retry_until 600 \
                    "Took too long for the provisioner to come up" \
                    provisioner_finished || exit 1
    fi
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
}
