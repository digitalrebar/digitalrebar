#!/bin/bash
# Copyright 2015, RackN Inc

function usage() {
  echo "Usage: $0 [--localhost] [--clean] [--identity <file>] [--user <user>] [--init-ident <file>] [--host] CIDRIP"
  exit 1
}

ID_FILE=""
CLEAN_IT=""
INIT_ID_FILE=""
ACCOUNT="--user root"
SUDO=""
HOST_MODE=""
LOCALHOST=""

while (( $# > 0 )); do
  arg="$1"
  case $arg in
    --clean) shift; CLEAN_IT="--clean";;
    --identity) shift; ID_FILE="--identiy $1"; shift;;
    --user) shift; ACCOUNT="--user $1"; shift;;
    --init-ident) shift; INIT_ID_FILE="-init-ident $1"; shift;;
    --host) shift ; HOST_MODE="YES";;
    --localhost) shift ; LOCALHOST="--connection=local";;
    --help|-h) usage;;
    *) break;;
  esac
done

CIDRIP=$1
IP=${CIDRIP%/*}
CIDR=${CIDRIP##*/}
if [ "$CIDRIP" == "" ] ; then
    echo "Please provide a CIDR IP"
    exit 1
fi

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

echo "Device IP = $IP/$CIDR"

ssh-keygen -f "~/.ssh/known_hosts" -R $IP
ssh -oBatchMode=yes -o StrictHostKeyChecking=no root@$IP date
if [ $? -ne 0 ]; then
    echo scripts/ssh-copy-id.sh $CLEAN_IT $ID_FILE $ACCOUNT $INIT_ID_FILE root@$IP
    scripts/ssh-copy-id.sh $CLEAN_IT $ID_FILE $ACCOUNT $INIT_ID_FILE root@$IP
fi

EXTRA_VARS=""
if [ "$HOST_MODE" == "YES" ] ; then
  EXTRA_VARS="dr_access_mode=HOST dr_external_ip=$IP/$CIDR"
fi

echo "$IP ansible_ssh_user=root" > /tmp/run-in-hosts.$$
export ANSIBLE_HOST_KEY_CHECKING=False
ansible-playbook -i /tmp/run-in-hosts.$$ --extra-vars "$EXTRA_VARS" digitalrebar.yml $LOCALHOST

echo "=== HELPFUL COMMANDS ==="
echo "repeat Ansible run: ansible-playbook -i /tmp/run-in-hosts.$$ --extra-vars \"$EXTRA_VARS\" digitalrebar.yml $LOCALHOST"
echo "Digital Rebar UI https://${IP}:3000"
