set -e

if [[ ! $ADMIN_HOSTNAME ]]; then

    ADMIN_HOSTNAMES=("cr0wbar.pwns.joo"
                     "vltima.ratio.regvm"
                     "omnia.fines.bon"
                     "admin.smoke.test"
                     "eheyeh.asher.eheyeh"
                     "bork.bork.bork")

    ADMIN_HOSTNAME=${ADMIN_HOSTNAMES[$(($RANDOM % ${#ADMIN_HOSTNAMES[@]}))]}
fi

debug() {
    printf '%s\n' "$@" >&2
}

die() {
    debug "$@"
    exit 1
}

# Launch an admin node without bringing Crowbar up.
launch_admin() {
    set +e
    if DOCKER_ID="$(core/tools/docker-admin --hostname "$ADMIN_HOSTNAME" --daemon --dont-map-ports centos)"; then
        DOCKER_IP="$(docker inspect $DOCKER_ID |jq -r '.[0] | .["NetworkSettings"] | .["IPAddress"]')"
        debug "Docker admin container at $DOCKER_ID"
        export DOCKER_ID
        trap clean_up EXIT
        set -e
        return 0
    fi
    die "Failed to launch docker admin container"
}

# Bring Crowbar up.
run_admin() {
    docker exec $DOCKER_ID /opt/opencrowbar/core/production.sh "$ADMIN_HOSTNAME" >&2
}

clean_up() {
    set +e
    pkill kvm_slave
    if [[ $DOCKER_ID ]]; then
        docker cp "$DOCKER_ID:/var/log/crowbar" .
        docker kill $DOCKER_ID
        docker rm $DOCKER_ID
    fi &>/dev/null
}

[[ -x $PWD/core/tools/docker-admin ]] || \
    die "Cannot find docker-admin in expected location $PWD/core/tools"

which jq &>/dev/null || die "We must have jq installed"
    

