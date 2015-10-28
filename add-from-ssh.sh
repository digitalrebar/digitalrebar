#!/bin/bash
# Copyright 2015, RackN Inc

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

CIDRIP=$1
IP=${CIDRIP%/*}
CIDR=${CIDRIP##*/}
if [ "$CIDRIP" == "" ] ; then
    echo "Please provide a CIDR IP"
    exit 1
fi

echo "Device ip = $IP/$CIDR"

ssh-keygen -f "~/.ssh/known_hosts" -R $IP
ssh -o StrictHostKeyChecking=no root@$IP date

date

# Get the ssh keys and update authorized_keys
REBAR_KEY="rebar:rebar1"
KEY_FILE1="/tmp/keys.$$"
KEY_FILE2="/tmp/keys2.$$"
success=$(curl -s -o $KEY_FILE1 -w "%{http_code}" --digest -u "$REBAR_KEY" \
      -X GET "http://$ADMIN_IP:3000/api/v2/deployments/1/attribs/rebar-access_keys")
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
