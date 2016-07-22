#!/bin/bash

run_forever() (
  while true; do
    "$@"
    sleep 5
  done
)

HOSTS=`hostname`
HOSTS="dhcp,dhcp-mgmt,dhcp-mgmt-service,$HOSTS,localhost"
I_IP=`ip addr show | grep inet | grep -v inet6 | awk '{ print $2 }' | awk -F/ '{ print $1 }'|sed "s/ /,/g"`
CI_IP=`echo $I_IP | sed "s/ /,/g"`
HOSTS="$HOSTS,$CI_IP"

# Service has to start first - or we should move the consul stuff into the app.
run_forever /usr/local/bin/rebar-dhcp --server_ip=$EXTERNAL_IP --backing_store=consul --data_dir=digitalrebar/dhcp/database --auth_mode=KEY --host="$HOSTS" &

# Add rev-proxy matcher
echo '^dhcp/(.*)' | kv_put digitalrebar/public/revproxy/dhcp-mgmt-service/matcher

make_service dhcp 67 '{"script": "pidof rebar-dhcp","interval": "10s"}'

OSD=$SERVICE_DEPLOYMENT
SERVICE_DEPLOYMENT="$OSD\", \"revproxy"
make_service dhcp-mgmt  6755 '{"script": "pidof rebar-dhcp","interval": "10s"}'
SERVICE_DEPLOYMENT=$OSD

