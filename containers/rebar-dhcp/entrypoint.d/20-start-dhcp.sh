#!/bin/bash

run_forever() (
  while true; do
    "$@"
    sleep 5
  done
)

DHCP_MGMT_PORT=${DHCP_MGMT_PORT:-6755}

HOSTS=`hostname`
HOSTS="dhcp,dhcp-mgmt,dhcp-mgmt-service,$HOSTS,localhost"
I_IP=`ip addr show | grep inet | grep -v inet6 | awk '{ print $2 }' | awk -F/ '{ print $1 }'|sed "s/ /,/g"`
CI_IP=`echo $I_IP | sed "s/ /,/g"`
HOSTS="$HOSTS,$CI_IP"

# Service has to start first - or we should move the consul stuff into the app.
run_forever /usr/local/bin/rebar-dhcp --port $DHCP_MGMT_PORT --serverIp=$EXTERNAL_IP --backingStore=consul --dataDir=digitalrebar/dhcp/database --host="$HOSTS" &
make_service dhcp 67 '{"script": "pidof rebar-dhcp","interval": "10s"}'
make_revproxied_service dhcp-mgmt  $DHCP_MGMT_PORT '{"script": "pidof rebar-dhcp","interval": "10s"}'

