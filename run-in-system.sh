#!/bin/bash
# Copyright 2015, RackN Inc

ERROR=""
if ! which ansible &>/dev/null; then
    echo "Please install Ansible!"
    ERROR="YES"
fi

if ! which jq &>/dev/null; then
    echo "Please install jq!"
    echo "Something like: brew install jq"
    ERROR="YES"
fi

if [ "$ERROR" == "YES" ] ; then
    exit 1
fi

# Default to the one that will always work.
HOST_MODE=""
if [ "$1" == "--host" ] ; then
  shift
  HOST_MODE="YES"
fi

CIDRIP=$1
IP=${CIDRIP%/*}
CIDR=${CIDRIP##*/}
if [ "$CIDRIP" == "" ] ; then
    echo "Please provide a CIDR IP"
    exit 1
fi

echo "Device IP = $IP/$CIDR"

ssh-keygen -f "~/.ssh/known_hosts" -R $IP
ssh -o StrictHostKeyChecking=no root@$IP date
if [ $? -ne 0 ]; then
    scripts/ssh-copy-id.sh root@$IP
fi

EXTRA_VARS=""
if [ "$HOST_MODE" == "YES" ] ; then
  EXTRA_VARS="dr_access_mode=HOST dr_external_ip=$IP/$CIDR"
fi

echo "$IP ansible_ssh_user=root" > run-in-hosts
export ANSIBLE_HOST_KEY_CHECKING=False
ansible-playbook -i run-in-hosts --extra-vars "$EXTRA_VARS" digitalrebar.yml

echo "=== HELPFUL COMMANDS ==="
echo "repeat Ansible run: ansible-playbook -i run-in-hosts --extra-vars \"$EXTRA_VARS\" digitalrebar.yml"
echo "Consul UI        http://${IP}:8500"
echo "Digital Rebar UI http://${IP}:3000"
