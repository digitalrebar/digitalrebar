#!/bin/env bash

die() {
    printf '%s\n' "$@"
    exit 1
}

if which sudo 2>/dev/null >/dev/null ; then
    SUDO=sudo
fi

if [ "${BASH_VERSINFO}" -lt 4 ] ; then
    die "Must have a bash version of 4 or higher"
fi

export PS4='${BASH_SOURCE}@${LINENO}(${FUNCNAME[0]}): '
if ! which docker &>/dev/null; then
    die "Please install Docker!"
fi

if (which sestatus && sestatus |egrep -q '^Current mode:.*enforcing') &>/dev/null; then
    die "SELinux is enforcing!  It does not play nice with some of our containers, please disable it."
fi

if ! which docker-compose &>/dev/null; then
    die "Please install docker-compose!"
fi

rebar() {
    docker exec compose_rebar_api_1 rebar -E https://127.0.0.1:3000 -U rebar -P rebar1 "$@"
}

converge() {
    rebar converge && return 0
    failed_ids=($(rebar noderoles match '{"state": -1}' |jq -r '.[] |.id'))
    if [[ ! $failed_ids ]]; then
        echo "Converge failed, but no noderoles errored!"
        return 1
    fi
    for id in "${failed_ids[@]}"; do
        failed_noderole="$(rebar noderoles show "$id")"
        nodename=$(rebar nodes show $(printf '%s' "$failed_noderole" |jq -r '.node_id') |jq -r -c '.name')
        rolename=$(rebar roles show $(printf '%s' "$failed_noderole" |jq -r '.role_id') |jq -r -c '.name')
        echo "*** FAILED NODEROLE $nodename: $rolename ***"
        printf '%s' "$failed_noderole" |jq -r '.runlog'
        echo
        echo "*** END FAILED NODEROLE $nodename: $rolename ***"
    done
    echo "Rebar failed to converge."
    return 1
}

known_containers=(provisioner dhcp ntp dns dns-mgmt revproxy chef webproxy logging debug node access)

declare -A containers

use_container() {
    ! [[ ! ${containers[$1]} || ${containers[$1]} == false ]]
}

docker_admin_default_containers() {
    [[ ${containers["provisioner"]} ]] || containers["provisioner"]=true
    [[ ${containers["dhcp"]} ]] || containers["dhcp"]=true
    [[ ${containers["dns"]} ]] || containers["dns"]=true
    [[ ${containers["dns-mgmt"]} ]] || containers["dns-mgmt"]=true
    [[ ${containers["ntp"]} ]] || containers["ntp"]=true
    [[ ${containers["chef"]} ]] || containers["chef"]=true
    [[ ${containers["webproxy"]} ]] || containers["webproxy"]=true
    [[ ${containers["revproxy"]} ]] || containers["revproxy"]=true
    [[ ${containers["access"]} ]] || containers["access"]=FORWARDER
}

args=()
branch="$(git symbolic-ref -q HEAD)"
branch="${branch##refs/heads/}"
branch="${branch:-latest}"

DR_TAG="${DR_TAG:-${branch}}"
export DR_TAG

while (( $# > 0 )); do
    arg="$1"
    case $arg in
        --no-pull) export NO_PULL="Y";;
        --dev) export DEV_MODE="Y";;
        --tag)
            export DR_TAG="$2"
            shift;;
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
    if [[ ! -d $mountdir/deploy/compose ]]; then
        echo "$mountdir/deploy does not contain a checkout of the digitalrebar container deployer!"
    elif [[ ! -L  $mountdir/deploy/compose/digitalrebar ]]; then
        $SUDO rm -rf "$mountdir/deploy/compose/digitalrebar"
        ln -s "$mountdir" \
           "$mountdir/deploy/compose/digitalrebar"
    fi

    if [[ $DEV_MODE = Y && ! -d $mountdir/digitalrebar-ux ]]; then
        # Clone the digitalrebar-ux repo to a known location.
        git clone https://github.com/rackn/digitalrebar-ux "$mountdir/digitalrebar-ux"
    fi

    mkdir -p "$HOME/.cache/digitalrebar/tftpboot"
    chmod 777 "$HOME/.cache/digitalrebar/tftpboot"

    $SUDO rm -rf "$HOME/.cache/digitalrebar/tftpboot/machines"
    if [[ -f $HOME/.ssh/id_rsa.pub ]]; then
        cp "$HOME/.ssh/id_rsa.pub" "$mountdir/deploy/compose/config-dir/api/config/ssh_keys/$USER-0.key"
    fi

    cd "$mountdir/deploy/compose"
    if (which selinuxenabled && which chcon) &>/dev/null && selinuxenabled; then
        $SUDO chcon -Rt svirt_sandbox_file_t "$mountdir"
        $SUDO chcon -Rt svirt_sandbox_file_t "$HOME/.cache/digitalrebar/tftpboot"
    fi

    [[ -d $mountdir/deploy/compose/data-dir ]] && $SUDO rm -rf "$mountdir/deploy/compose/data-dir"
    ./init_files.sh --clean
    make_compose_args
    ./init_files.sh $REAL_COMPOSE_ARGS

    if [ "$NO_PULL" == "" ] ; then
        docker-compose pull
    fi
    if [[ ${containers["access"]} = FORWARDER && $(uname -s) != Darwin && $(uname -s) != "MINGW64_NT-10.0" ]]; then
        $SUDO modprobe nf_nat_tftp
    fi
    docker-compose up -d
    # enable development mode
    if [ "$DEV_MODE" == "Y" ] ; then
        docker exec -it compose_rebar_api_1 touch /opt/digitalrebar/dev.mode
        echo "Starting in DEVELOPMENT MODE"
    fi
}

test_phantom() {
    ! rebar nodes show system-phantom.internal.local | jq '.alive, .available' |grep false
}

backup_containers() {
    backup_location=$(mktemp -d /tmp/rebar-backup-XXXXXXXX)
    rm -rf "$backup_location"
    (cd "$mountdir/deploy" && ./backup.sh  "$backup_location")
}

restore_containers() {
    [[ -d $backup_location ]] || die "No backup location"
    (cd "$mountdir/deploy" && ./restore.sh "$backup_location")
}

wait_for_admin_containers() {
    echo "Waiting on API to start (up to 240 seconds)"
    retry_until 240 \
                "Took too long for rebar container to come up" \
                rebar ping || exit 1

    echo "Waiting on system deployment"
    retry_until 240 \
                "Took too long for system deployment to appear" \
                rebar deployments show system || exit 1
    echo "Waiting on system-phantom.internal.local"
    retry_until 240 \
                "Took too long for system-phantom.internal.local to be runnable" \
                test_phantom || exit 1
    echo "Waiting for rebar to converge (up to 10 minutes)"
    converge || exit 1
}

tear_down_admin_containers() {
    cd "$mountdir/deploy/compose"
    docker-compose stop
    docker-compose rm -f
}
