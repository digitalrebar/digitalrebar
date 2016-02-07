#!/usr/bin/env bash
# Copyright 2016, RackN Inc

#
# We are deploying in aws add this aws instance as a provider
# and force the admin deploy to aws
#
FORCE_PROVIDER=${PROVIDER:-docean}
FORCE_DEPLOY_ADMIN=${DEPLOY_ADMIN:-docean}


if ! which tugboat &>/dev/null; then
    echo "Please install Digital Ocean Tugboat (gem install tugboat then tugboat authorize)!"
    exit 1
fi


# Processes args, inits provider, and validates provider
. workloads/wl-lib.sh

PROVIDER_DOCEAN_ADMIN_INSTANCE_TYPE=${PROVIDER_DOCEAN_ADMIN_INSTANCE_TYPE:-'8gb'}
if [[ $PROVIDER_DOCEAN_REGION ]] ; then
    ZONE_NAME="$PROVIDER_DOCEAN_REGION"
fi

# Check to see if device id exists.
if [ "$DEVICE_ID" != "" ] ; then
    STATE=`tugboat droplets | grep --color=never $DEVICE_ID | sed -r "s:\x1B\[[0-9;]*[mK]::g" | sed -n 's/(.*\) status: \([a-z]*\)\, \(.*\)/\2/p'`
    NODENAME=`tugboat droplets | grep --color=never $DEVICE_ID | sed -r "s:\x1B\[[0-9;]*[mK]::g" | sed -n 's/\(.*\) (ip: \(.*\)/\1/p'`
    if [[ $STATE == null ]] ; then
        echo "Instance ID doesn't exist in DOcean: $DEVICE_ID"
        exit 1
    fi
    echo "DOCEAN reuse ${DEVICE_ID}"
else
    # Make name for unamed items
    NODENAME=$1
    if [ "$NODENAME" == "" ] ; then
        TSTAMP=`date +%H%M`
        NODENAME="${USER}1-${TSTAMP}"
    else
        shift
    fi
    echo "DOCEAN will create ${NODENAME}"

    # This defaults to debian 8 - just do it for now.
    if ! tugboat create ${NODENAME} -r ${ZONE_NAME} -s ${PROVIDER_DOCEAN_ADMIN_INSTANCE_TYPE} ; then
        echo "Failed to create tugboat instance"
        exit 1
    fi
    sleep 5
    DEVICE_ID=`tugboat droplets | grep --color=never ${NODENAME} | sed -r "s:\x1B\[[0-9;]*[mK]::g" | sed -n 's/\(.*\)\, id: \([0-9]*\))/\2/p'`
fi

# Wait for device to be up
STATE=`tugboat droplets | grep --color=never ${NODENAME} | sed -r "s:\x1B\[[0-9;]*[mK]::g"   | sed -n 's/\(.*\) status: \([a-z]*\)\, \(.*\)/\2/p'`
while [ "$STATE" != "active" ] ; do
  echo "STATE = $STATE"
  sleep 5
  STATE=`tugboat droplets | grep --color=never ${NODENAME} | sed -r "s:\x1B\[[0-9;]*[mK]::g" | sed -n 's/\(.*\) status: \([a-z]*\)\, \(.*\)/\2/p'`
done

# Get Public IP 
IP=`tugboat droplets | grep --color=never ${NODENAME} | sed -r "s:\x1B\[[0-9;]*[mK]::g" | sed -n 's/\(.*\)(ip: \([0-9.]*\)\, \(.*\)/\2/p'`
CIDR=32

ENV_VAR="\"docean\": true,"

export ADMIN_IP="$IP/$CIDR"

# Make sure our key is in place.
# WIP > tugboat keys  $ZONE_NAME root@$DEVICE_ID --ssh-key-file $HOME/.ssh/id_rsa --command "date"

. ./run-in-system.sh

echo "DOcean Device ID: $DEVICE_ID"
echo "repeat DOcean run: ./run-in-docean.sh --device-id=${DEVICE_ID}"
echo "SSH access: ssh -X root@${IP}"
