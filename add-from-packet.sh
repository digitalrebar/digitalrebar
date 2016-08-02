#!/bin/bash
# Copyright 2015, RackN Inc

# Include Project API Key in Packet
. ~/.dr_info

if [ "$PROVIDER_PACKET_KEY" == "" ] ; then
    echo "You must define PROVIDER_PACKET_KEY (can be added to ~/.dr_info)"
    exit 1
fi

if [ "$PROVIDER_PACKET_PROJECT_ID" == "" ] ; then
    echo "You must define PROVIDER_PACKET_PROJECT_ID (can be added to ~/.dr_info)"
    exit 1
fi

if ! which jq &>/dev/null; then
    echo "Please install jq!"
    exit 1
fi

if [ "$1" == "--admin-ip" ] ; then
    shift
    ADMIN_IP=$1
    shift
else
    echo "Must specify an Admin IP"
    exit 1
fi

if [ "$1" == "--device-id" ] ; then
  shift
  DEVICE_ID="$1"
  shift
else
  NODENAME=$1
  if [ "$NODENAME" == "" ] ; then
    NODENAME="${USER}1"
    shift
  fi
fi


TSTAMP=`date +%H%M`

#Only works if there's only one project in your packet.net account
#or to which your user is associated, better to add it to dr_info
#PROVIDER_PACKET_PROJECT_ID=`curl -s -H "X-Auth-Token: $PROVIDER_PACKET_KEY" https://api.packet.net/projects | jq -r ".projects[].id"`

echo "My projects: ${PROVIDER_PACKET_PROJECT_ID} will be called ${NODENAME}_at_${TSTAMP}"

#curl -s -H "X-Auth-Token: $PROVIDER_PACKET_KEY" https://api.packet.net/plans | jq .
#curl -s -H "X-Auth-Token: $PROVIDER_PACKET_KEY" https://api.packet.net/operating-systems | jq .

# Example OS lines for packet
#  \"operating_system\": \"ubuntu_14_04\",
#  \"operating_system\": \"centos_7\",

node="{
  \"facility\": \"ewr1\",
  \"plan\": \"baremetal_1\",
  \"operating_system\": \"ubuntu_14_04\",
  \"hostname\": \"${NODENAME}${TSTAMP}\"
}"

date

if [ "$DEVICE_ID" == "" ] ; then
  DEVICE_ID=`curl -H "Content-Type: application/json" -X POST --data "$node" -H "X-Auth-Token: $PROVIDER_PACKET_KEY" https://api.packet.net/projects/$PROVIDER_PACKET_PROJECT_ID/devices | jq -r .id`
fi

STATE=`curl -s -H "X-Auth-Token: $PROVIDER_PACKET_KEY" https://api.packet.net/projects/$PROVIDER_PACKET_PROJECT_ID/devices/$DEVICE_ID | jq -r .state`
while [ "$STATE" != "active" ] ; do
  echo "STATE = $STATE"
  sleep 5
  STATE=`curl -s -H "X-Auth-Token: $PROVIDER_PACKET_KEY" https://api.packet.net/projects/$PROVIDER_PACKET_PROJECT_ID/devices/$DEVICE_ID | jq -r .state`
done

date

# Get Public IP - HACK - should look it up
IP=`curl -s -H "X-Auth-Token: $PROVIDER_PACKET_KEY" https://api.packet.net/projects/$PROVIDER_PACKET_PROJECT_ID/devices/$DEVICE_ID | jq -r .ip_addresses[0].address`
CIDR=`curl -s -H "X-Auth-Token: $PROVIDER_PACKET_KEY" https://api.packet.net/projects/$PROVIDER_PACKET_PROJECT_ID/devices/$DEVICE_ID | jq -r .ip_addresses[0].cidr`

echo "Device ip = $IP/$CIDR"

ssh-keygen -f "~/.ssh/known_hosts" -R $IP
ssh -o StrictHostKeyChecking=no root@$IP date

date

# Get the ssh keys and update authorized_keys
REBAR_ENDPOINT="https://$ADMIN_IP:3000"
REBAR_KEY="rebar:rebar1"
KEY_FILE1="/tmp/keys.$$"
KEY_FILE2="/tmp/keys2.$$"
success=$(curl -k -s -o $KEY_FILE1 -w "%{http_code}" --digest -u "$REBAR_KEY" \
      -X GET "${REBAR_ENDPOINT}/api/v2/deployments/1/attribs/rebar-access_keys")
if [[ $success != 200 ]] ; then
    echo "Failed to get keys"
    exit -1
fi
jq -r '.value|to_entries[].value' $KEY_FILE1 > $KEY_FILE2
scp $KEY_FILE2 root@$IP:keys
rm -rf $KEY_FILE1 $KEY_FILE2

scp scripts/join_rebar.sh root@$IP:
ssh root@$IP /root/join_rebar.sh $ADMIN_IP

echo "=== HELPFUL COMMANDS ==="
echo "Packet Device ID: $DEVICE_ID"
echo "SSH: ssh root@$IP"
