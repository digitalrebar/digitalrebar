#!/bin/bash
export PS4='${BASH_SOURCE}@${LINENO}(${FUNCNAME[0]}): '
set -e
set -x
set -o pipefail

kv_get() {
    # $1 = path into the KV store of the thing to get
    curl -sfg "http://localhost:8500/v1/kv/${1}?token=${CONSUL_M_ACL}" | \
        jq -r -c '.[0].Value' | \
        base64 -d
}

kv_put() {
    # $1 = path into the KV store to put things
    # Stdin should contain the data to store
    curl -sfg "http://localhost:8500/v1/kv/${1}?token=${CONSUL_M_ACL}" \
         -X PUT \
         --data-binary @-
}

get_service() {
    # $1 = service name.
    curl http://localhost:8500/v1/catalog/service/$1
}

rebar() {
    command rebar -T -U system "$@"
}

# This should only be called in a subshell where we want a command to
# be able to talk to the outside world.  It relies the proxy being registered and alive,
# and it will wait until it is before proceeding.
with_local_proxy() {
    if [[ ! $(get_service consul) ]]; then
        echo "with_local_proxy called before consul is up!"
        echo "$0 must execute after 10 in the entrypoint script ordering"
        echo "This is a deadlock situation"
        sleep 600
        exit 1
    fi
    local blob=""
    while true; do
        local blob=$(get_service proxy-service)
        [[ $blob && $blob != '[]' ]] && break
        sleep 5
    done
    local addr=$(jq -r '.[0] |.ServiceAddress' <<< "$blob")
    [[ $addr ]] || addr=$(jq '.[0] |.Address' <<< "$blob")
    local port=$(jq -r '.[0] |.ServicePort' <<< "$blob")
    export HTTP_PROXY="http://$addr:$port"
    export HTTPS_PROXY="$HTTP_PROXY"
    export http_proxy="$HTTP_PROXY"
    export https_proxy="$HTTP_PROXY"
}

make_service() {
    # $1 = service name.
    # $2 = port to check on
    # $3 = check fragment%s
    if [[ $FORWARDER_IP ]]; then
        printf '{"name":"internal-%s-service","port":%s,"tags":["deployment:%s"],"check":%s}' "$1" "$2" "$SERVICE_DEPLOYMENT" "$3"
    else
        printf '{"name":"%s-service","port":%s,"address":"%s","tags":["deployment:%s"],"check":%s}' "$1" "$2" "${EXTERNAL_IP%%/*}" "$SERVICE_DEPLOYMENT" "$3"
    fi | curl -X PUT http://localhost:8500/v1/agent/service/register --data-binary @-
}

make_revproxied_service() {
    # $1 = service name.
    # $2 = port to check on
    # $3 = check fragment
    local name="${1%-mgmt}"
    printf '^%s/(.*)' "$name" |kv_put "digitalrebar/public/revproxy/${1}-service/matcher"
    printf '{"name":"%s-service","port":%s,"tags":["revproxy", "deployment:%s"],"check":%s}' "$1" "$2" "$SERVICE_DEPLOYMENT" "$3" | \
        curl -X PUT http://localhost:8500/v1/agent/service/register --data-binary @-
    
}

bind_service() {
    # $1 = service to bind to the system deployment
    local json='' deployment_id role_id dr_id
    deployment_id=$(rebar deployments show $SERVICE_DEPLOYMENT |jq -r '.id')
    role_id=$(rebar roles show $1 |jq -r '.id')
    dr_id=$(rebar deploymentroles match "{\"role_id\": $role_id, \"deployment_id\": $deployment_id}" |
                   jq -r '.[0].id')
    if [[ $dr_id = null ]]; then
        if json=$(rebar nodes bind $SERVICE_DEPLOYMENT-phantom.internal.local to "$1"); then
            rebar noderoles commit $(jq -r '.id' <<< "$json")
        fi
    fi
}

set_service_attrib() {
    # $1 = service name
    # $2 = attrib name
    # $3 = attrib value
    local json=''
    local deployment_id role_id dr_id
    deployment_id=$(rebar deployments show $SERVICE_DEPLOYMENT |jq -r '.id')
    role_id=$(rebar roles show $1 |jq -r '.id')
    dr_id=$(rebar deploymentroles match "{\"role_id\": $role_id, \"deployment_id\": $deployment_id}" |
                   jq -r '.[0].id')
    rebar deploymentroles set "$dr_id" attrib "$2" to "$3" && rebar deploymentroles commit "$dr_id"
}

unset answer count
# Just about everything wants an IP address.  Get the first global one
IP=$(ip -o -4 addr | grep 'scope global' | awk '{ print $4 }' | awk -F/ '{ print $1 }' | head -1)
forwarder=$(awk '/forwarder/ { print $1}' </etc/hosts|head -1)

# Now that consul is up, run whatever commands we want the system to run.
for cmd in /usr/local/entrypoint.d/*.sh; do
    [[ -x $cmd ]] || continue
    echo "Calling cmd: $cmd"
    . "$cmd"
done
