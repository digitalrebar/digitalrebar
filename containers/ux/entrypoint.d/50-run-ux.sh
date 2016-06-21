#!/bin/bash

run_forever() (
    while true; do
        "$@"
        sleep 5
    done
)

if [[ $FORWARDER_IP && $forwarder ]]; then
    ip route del default
    ip route add default via $forwarder
    sleep 30
fi

if [[ ! $UX_PORT ]] ; then
    UX_PORT=8443
fi

# Add rev-proxy matcher
echo '^ux/(.*)' | kv_put digitalrebar/public/revproxy/rebar-ux-service/matcher

OSD=$SERVICE_DEPLOYMENT
SERVICE_DEPLOYMENT="$OSD\", \"revproxy"
make_service "rebar-ux" $UX_PORT "{\"script\": \"curl -k -H \\\"Host=www.mydomain.com\\\" https://localhost:$UX_PORT\",\"interval\": \"10s\"}"
SERVICE_DEPLOYMENT=$OSD

cd /opt/digitalrebar-ux

bower --allow-root install --config.interactive=false

EIP=${EXTERNAL_IP%/*}
FIP=${FORWARDER_IP%/*}

HOSTS=`hostname`
HOSTS="rebar-ux-service,$HOSTS,localhost"
I_IP=`ip addr show | grep inet | grep -v inet6 | awk '{ print $2 }' | awk -F/ '{ print $1 }'|sed "s/ /,/g"`
CI_IP=`echo $I_IP | sed "s/ /,/g"`
HOSTS="$HOSTS,${EIP:-127.0.1.1},${FIP:-127.0.2.1},$CI_IP"

generate_crt "server" "rebar-ux" "$HOSTS"

touch websecureport.log
run_forever python simple-https.py $UX_PORT >websecureport.log 2>&1 &

tail -f websecureport.log
