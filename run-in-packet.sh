#!/bin/bash
# Copyright 2015, RackN Inc

# Include Project API Key in Packet
. ~/.dr_info

if [ "$API_KEY" == "" ] ; then
    echo "You must define API_KEY (can be added to ~/.dr_info)"
    exit 1
fi

if ! which ansible &>/dev/null; then
    echo "Please install Ansible!"
    exit 1
fi

if ! which jq &>/dev/null; then
    echo "Please install jq!"
    exit 1
fi

HOST_MODE=""
if [ "$1" == "--host" ] ; then
  shift
  HOST_MODE="YES"
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

PROJ_ID=`curl -s -H "X-Auth-Token: $API_KEY" https://api.packet.net/projects | jq -r ".projects[].id"`

echo "My projects: ${PROJ_ID} will be called ${NODENAME}_at_${TSTAMP}"

#curl -s -H "X-Auth-Token: $API_KEY" https://api.packet.net/plans | jq .
#curl -s -H "X-Auth-Token: $API_KEY" https://api.packet.net/operating-systems | jq .

node="{
  \"facility\": \"ewr1\",
  \"plan\": \"baremetal_1\",
  \"operating_system\": \"ubuntu_14_04\",
  \"hostname\": \"${NODENAME}${TSTAMP}\"
}"

date

if [ "$DEVICE_ID" == "" ] ; then
  DEVICE_ID=`curl -H "Content-Type: application/json" -X POST --data "$node" -H "X-Auth-Token: $API_KEY" https://api.packet.net/projects/$PROJ_ID/devices | jq -r .id`
fi

STATE=`curl -s -H "X-Auth-Token: $API_KEY" https://api.packet.net/projects/$PROJ_ID/devices/$DEVICE_ID | jq -r .state`
while [ "$STATE" != "active" ] ; do
  echo "STATE = $STATE"
  sleep 5
  STATE=`curl -s -H "X-Auth-Token: $API_KEY" https://api.packet.net/projects/$PROJ_ID/devices/$DEVICE_ID | jq -r .state`
done

date

# Get Public IP - HACK - should look it up
IP=`curl -s -H "X-Auth-Token: $API_KEY" https://api.packet.net/projects/$PROJ_ID/devices/$DEVICE_ID | jq -r .ip_addresses[0].address`
CIDR=`curl -s -H "X-Auth-Token: $API_KEY" https://api.packet.net/projects/$PROJ_ID/devices/$DEVICE_ID | jq -r .ip_addresses[0].cidr`

echo "Device ip = $IP/$CIDR"

ssh-keygen -f "~/.ssh/known_hosts" -R $IP
ssh -o StrictHostKeyChecking=no root@$IP date

# Build ansible inventory file
echo "$IP ansible_ssh_user=root" > run-in-hosts

EXTRA_VARS=""
if [ "$HOST_MODE" == "YES" ] ; then
  EXTRA_VARS="dr_access_mode=HOST dr_external_ip=$IP/$CIDR"
fi

export ANSIBLE_HOST_KEY_CHECKING=False
ansible-playbook -i run-in-hosts --extra-vars "$EXTRA_VARS" digitalrebar.yml

date

echo "=== HELPFUL COMMANDS ==="
echo "Packet Device ID: $DEVICE_ID"
echo "repeat Ansible run: ansible-playbook -i run-in-hosts --extra-vars \"$EXTRA_VARS\" digitalrebar.yml"
echo "SSH access: ssh -X root@${IP}"
echo "Consul UI        http://${IP}:8500"
echo "Digital Rebar UI http://${IP}:3000"
