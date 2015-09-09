#!/bin/bash

# Include RackN Project API Key in Packet
. ~/.dr_info

NODENAME=$1
if [ "$NODENAME" == "" ] ; then
  NODENAME="$USER1"
fi

PROJ_ID=`curl -s -H "X-Auth-Token: $API_KEY" https://api.packet.net/projects | jq -r ".projects[].id"`

echo "My projects: $PROJ_ID"

#curl -s -H "X-Auth-Token: $API_KEY" https://api.packet.net/plans | jq .
#curl -s -H "X-Auth-Token: $API_KEY" https://api.packet.net/operating-systems | jq .

node="{
  \"facility\": \"ewr1\",
  \"plan\": \"baremetal_1\",
  \"operating_system\": \"ubuntu_14_04\",
  \"hostname\": \"$NODENAME\"
}"

DEVICE_ID=`curl -H "Content-Type: application/json" -X POST --data "$node" -H "X-Auth-Token: $API_KEY" https://api.packet.net/projects/$PROJ_ID/devices | jq -r .id`

STATE=`curl -s -H "X-Auth-Token: $API_KEY" https://api.packet.net/projects/$PROJ_ID/devices/$DEVICE_ID | jq -r .state`
while [ "$STATE" != "active" ] ; do
  echo "STATE = $STATE"
  sleep 5
  STATE=`curl -s -H "X-Auth-Token: $API_KEY" https://api.packet.net/projects/$PROJ_ID/devices/$DEVICE_ID | jq -r .state`
done

# Get Public IP - HACK - should look it up
IP=`curl -s -H "X-Auth-Token: $API_KEY" https://api.packet.net/projects/$PROJ_ID/devices/$DEVICE_ID | jq -r .ip_addresses[0].address`

echo "Device ip = $IP"

ssh-keygen -f "~/.ssh/known_hosts" -R $IP
ssh -o StrictHostKeyChecking=no root@$IP date

# Build ansible inventory file
echo "$IP ansible_ssh_user=root" > run-in-hosts

export ANSIBLE_HOST_KEY_CHECKING=False
ansible-playbook -i run-in-hosts ubuntu1404-compose.yml

