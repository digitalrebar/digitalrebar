#!/usr/bin/env bash
# Copyright 2015, RackN Inc

# Still helper commands for finding values.
#curl -s -H "X-Auth-Token: $PROVIDER_PACKET_KEY" https://api.packet.net/plans
#curl -s -H "X-Auth-Token: $PROVIDER_PACKET_KEY" https://api.packet.net/operating-systems

#
# We are deploying in packet add this packet instance as a provider
# and force the admin deploy to packet
#
PROVIDER=${PROVIDER:-packet}
DEPLOY_ADMIN=${DEPLOY_ADMIN:-packet}

# Processes args, inits provider, and validates provider
. workloads/wl-lib.sh

# Check to see if device id exists.
if [ "$DEVICE_ID" != "" ] ; then
    STATE=`curl -s -H "X-Auth-Token: $PROVIDER_PACKET_KEY" https://api.packet.net/projects/$PROVIDER_PACKET_PROJECT_ID/devices/$DEVICE_ID | jq -r .state`

    if [[ $STATE == null ]] ; then
        echo "Device ID doesn't exist in packet: $DEVICE_ID"
        exit 1
    fi
    echo "My projects: ${PROVIDER_PACKET_PROJECT_ID} will reuse ${DEVICE_ID}"
else
    echo "My projects: ${PROVIDER_PACKET_PROJECT_ID} will create ${NODENAME}"

    # Make name for unamed items
    NODENAME=$1
    if [ "$NODENAME" == "" ] ; then
        TSTAMP=`date +%H%M`
        NODENAME="${USER}1-${TSTAMP}"
    else
        shift
    fi

    node="{
  \"facility\": \"ewr1\",
  \"plan\": \"baremetal_1\",
  \"operating_system\": \"ubuntu_14_04\",
  \"hostname\": \"${NODENAME}\"
}"

    DEVICE_ID=`curl -H "Content-Type: application/json" -X POST --data "$node" -H "X-Auth-Token: $PROVIDER_PACKET_KEY" https://api.packet.net/projects/$PROVIDER_PACKET_PROJECT_ID/devices | jq -r .id`
fi

# Wait for device to be up
STATE=`curl -s -H "X-Auth-Token: $PROVIDER_PACKET_KEY" https://api.packet.net/projects/$PROVIDER_PACKET_PROJECT_ID/devices/$DEVICE_ID | jq -r .state`
while [ "$STATE" != "active" ] ; do
  echo "STATE = $STATE"
  sleep 5
  STATE=`curl -s -H "X-Auth-Token: $PROVIDER_PACKET_KEY" https://api.packet.net/projects/$PROVIDER_PACKET_PROJECT_ID/devices/$DEVICE_ID | jq -r .state`
done

# We sleep here because while the API says up, not all services have started.
sleep 30

# Get Public IP - HACK - should look it up
IP=`curl -s -H "X-Auth-Token: $PROVIDER_PACKET_KEY" https://api.packet.net/projects/$PROVIDER_PACKET_PROJECT_ID/devices/$DEVICE_ID | jq -r .ip_addresses[0].address`
CIDR=`curl -s -H "X-Auth-Token: $PROVIDER_PACKET_KEY" https://api.packet.net/projects/$PROVIDER_PACKET_PROJECT_ID/devices/$DEVICE_ID | jq -r .ip_addresses[0].cidr`

ENV_VAR="\"packet\": true,"

export ADMIN_IP="$IP/$CIDR"

. ./run-in-system.sh

echo "Packet Device ID: $DEVICE_ID"
echo "repeat Packet run: ./run-in-packet.sh --device-id=${DEVICE_ID} ${NODENAME}"
echo "SSH access: ssh -X root@${IP}"
