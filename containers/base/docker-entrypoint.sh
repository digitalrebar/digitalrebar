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

generate_crt() {
    # $1 = cert name
    # $2 = CN
    # $3 = hosts (comma separated)

    local HOSTS=""
    if [[ $3 != "" ]] ; then
      IFS=', ' read -r -a array <<< "$3"
      unset IFS

      HOSTS="\"hosts\": ["
      local COMMA=""
      for element in "${array[@]}"
      do
          HOSTS="$HOSTS$COMMA \"$element\""
	  COMMA=","
      done
      HOSTS="$HOSTS ],"
    fi

cat > /tmp/csr.$$ <<EOF
{
  "CN": "$2",
  $HOSTS
  "key": {
    "algo": "rsa",
    "size": 2048
  },
  "names": [
    {
      "C": "US",
      "L": "Austin",
      "O": "RackN",
      "OU": "$2",
      "ST": "Texas"
    }
  ]
}
EOF
    cfssl genkey /tmp/csr.$$ | cfssljson -bare $1
    local inner_request="{ \"certificate_request\": \"$(cat $1.csr | tr '\n' '`' | sed 's/`/\\n/g')\" }"
    local token=$(echo -n "$inner_request" | openssl sha256 -mac hmac -macopt "hexkey:$CERT_AUTH_KEY" -binary | base64 -w0)
    local request="{ \"token\": \"$token\", \"request\": \"$(echo -n "$inner_request" | base64 -w0)\" }"

    # Get service info
    local tm_addr=$(get_service "trust-me-service" | jq -r .[0].ServiceAddress)
    local tm_port=$(get_service "trust-me-service" | jq -r .[0].ServicePort)

    if [[ $tm_addr == "" || $tm_addr == "null" ]] ; then
        tm_addr=$(get_service "trust-me-service" | jq -r .[0].Address)
    fi
    if [[ $tm_addr == "" || $tm_addr == "null" ]] ; then
        echo "Failed to find address of trust_me!"
        exit 1
    fi

    curl -s -d"$request" -X POST http://$tm_addr:$tm_port/api/v1/cfssl/authsign | cfssljson $1
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
