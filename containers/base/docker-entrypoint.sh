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

make_service() {
    # $1 = service name.
    # $2 = port to check on
    # $3 = check fragment
    if [[ $FORWARDER_IP ]]; then
        printf '{"name":"internal-%s-service","port":%s,"tags":["deployment:%s"],"check":%s}' "$1" "$2" "$SERVICE_DEPLOYMENT" "$3"
    else
        printf '{"name":"%s-service","port":%s,"address":"%s","tags":["deployment:%s"],"check":%s}' "$1" "$2" "${EXTERNAL_IP%%/*}" "$SERVICE_DEPLOYMENT" "$3"
    fi | curl -X PUT http://localhost:8500/v1/agent/service/register --data-binary @-
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
