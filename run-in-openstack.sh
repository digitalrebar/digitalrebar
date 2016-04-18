#!/usr/bin/env bash
# Copyright 2016, RackN Inc

#
# We are deploying in aws add this aws instance as a provider
# and force the admin deploy to aws
#
FORCE_PROVIDER=${PROVIDER:-openstack}
FORCE_DEPLOY_ADMIN=${DEPLOY_ADMIN:-openstack}


if ! which openstack &>/dev/null; then
    echo "Please install OpenStack CLI (apt-get install python-openstackclient)!"
    exit 1
fi

if ! which neutron &>/dev/null; then
    echo "Please install Neutron CLI (apt-get install python-neutronclient)!"
    exit 1
fi

# Processes args, inits provider, and validates provider
. workloads/wl-lib.sh

PROVIDER_OS_ADMIN_INSTANCE_TYPE=${PROVIDER_OS_ADMIN_INSTANCE_TYPE:-'8096'}
if [[ $PROVIDER_OS_REGION ]] ; then
    ZONE_NAME="$PROVIDER_OS_REGION"
fi

die "WIP"

# Check to see if device id exists.
if [ "$DEVICE_ID" != "" ] ; then
    STATE=`openstack compute show '$DEVICE_ID' -f shell
    NODENAME=`openstack server | grep --color=never $DEVICE_ID | sed -r "s:\x1B\[[0-9;]*[mK]::g" | sed -n 's/\(.*\) (ip: \(.*\)/\1/p'`
    if [[ $STATE == null ]] ; then
        echo "Instance ID doesn't exist in openstack: $DEVICE_ID"
        exit 1
    fi
    echo "openstack reuse ${DEVICE_ID}"
else
    # Make name for unamed items
    NODENAME=$1
    if [ "$NODENAME" == "" ] ; then
        TSTAMP=`date +%H%M`
        NODENAME="${USER}1-${TSTAMP}"
    else
        shift
    fi
    echo "openstack will create ${NODENAME}"

    # This defaults to debian 8 - just do it for now.
    if ! tugboat create ${NODENAME} -r ${ZONE_NAME} -s ${PROVIDER_OS_ADMIN_INSTANCE_TYPE} ; then
        echo "Failed to create tugboat instance"
        exit 1
    fi
    sleep 5
    DEVICE_ID=`openstack server | grep --color=never ${NODENAME} | sed -r "s:\x1B\[[0-9;]*[mK]::g" | sed -n 's/\(.*\)\, id: \([0-9]*\))/\2/p'`
fi

# Wait for device to be up
STATE=`openstack server | grep --color=never ${NODENAME} | sed -r "s:\x1B\[[0-9;]*[mK]::g"   | sed -n 's/\(.*\) status: \([a-z]*\)\, \(.*\)/\2/p'`
while [ "$STATE" != "active" ] ; do
  echo "STATE = $STATE"
  sleep 5
  STATE=`openstack server | grep --color=never ${NODENAME} | sed -r "s:\x1B\[[0-9;]*[mK]::g" | sed -n 's/\(.*\) status: \([a-z]*\)\, \(.*\)/\2/p'`
done

# Get Public IP 
IP=`openstack server | grep --color=never ${NODENAME} | sed -r "s:\x1B\[[0-9;]*[mK]::g" | sed -n 's/\(.*\)(ip: \([0-9.]*\)\, \(.*\)/\2/p'`
CIDR=32

ENV_VAR="\"openstack\": true,"

export ADMIN_IP="$IP/$CIDR"

# Make sure our key is in place.
# WIP > tugboat keys  $ZONE_NAME root@$DEVICE_ID --ssh-key-file $HOME/.ssh/id_rsa --command "date"

. ./run-in-system.sh

echo "openstack Device ID: $DEVICE_ID"
echo "repeat openstack run: ./run-in-openstack.sh --device-id=${DEVICE_ID}"
echo "SSH access: ssh -X root@${IP}"
