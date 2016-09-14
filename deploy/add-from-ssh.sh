#!/bin/bash
# Copyright 2015, RackN Inc

function usage() {
      echo "Usage: $0 [--identity <identity_file>] [--clean] [--init-ident <file>] --admin-ip <Admin IP> <CIDR of Node>" >&2
      exit 1
}

ID_FILE=""
CLEAN_IT=""
INIT_ID_FILE=""
ACCOUNT="--user root"
SUDO=""

while (( $# > 0 )); do
  arg="$1"
  case $arg in
    --admin-ip) shift; ADMIN_IP=$1; shift;;
    --clean) shift; CLEAN_IT="--clean";;
    --identity) shift; ID_FILE="--identiy $1"; shift;;
    --user) shift; ACCOUNT="--user $1"; shift;;
    --init-ident) shift; INIT_ID_FILE="-init-ident $1"; shift;;
    --help|-h) usage;;
    *) break;;
  esac
done

if [ "$ADMIN_IP" == "" ] ; then
    echo "Must specify an Admin IP"
    exit 1
fi

CIDRIP=$1
IP=${CIDRIP%/*}
CIDR=${CIDRIP##*/}
if [ "$CIDRIP" == "" ] ; then
    echo "Please provide a CIDR IP"
    exit 1
fi

if ! which jq &>/dev/null; then
    echo "Please install jq!"
    exit 1
fi

echo "Device ip = $IP/$CIDR"

ssh-keygen -f "~/.ssh/known_hosts" -R $IP
ssh -oBatchMode=yes -o StrictHostKeyChecking=no root@$IP date
if [ $? -ne 0 ]; then
    scripts/ssh-copy-id.sh $CLEAN_IT $ID_FILE $ACCOUNT $INIT_ID_FILE root@$IP
fi

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
