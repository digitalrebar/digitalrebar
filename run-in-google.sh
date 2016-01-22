#!/usr/bin/env bash
# Copyright 2015, RackN Inc

#
# We are deploying in aws add this aws instance as a provider
# and force the admin deploy to aws
#
FORCE_PROVIDER=${PROVIDER:-google}
FORCE_DEPLOY_ADMIN=${DEPLOY_ADMIN:-google}

# Processes args, inits provider, and validates provider
. workloads/wl-lib.sh

# Check to see if device id exists.
if [ "$DEVICE_ID" != "" ] ; then
    STATE=`gcloud compute instances describe $DEVICE_ID --format=json | jq -r .status`

    if [[ $STATE == null ]] ; then
        echo "Instance ID doesn't exist in aws: $DEVICE_ID"
        exit 1
    fi
    echo "GOOGLE reuse ${DEVICE_ID}"
else
    # Make name for unamed items
    NODENAME=$1
    if [ "$NODENAME" == "" ] ; then
        TSTAMP=`date +%H%M`
        NODENAME="${USER}1-${TSTAMP}"
    else
        shift
    fi
    echo "GOOGLE will create ${NODENAME}"
    DEVICE_ID=${NODENAME%%.*}

    # This defaults to debian 8 - just do it for now.
    if ! gcloud compute instances create ${DEVICE_ID} ; then
        echo "Failed to create gcloud instance"
        exit 1
    fi
fi

# Wait for device to be up
STATE=`gcloud compute instances describe $DEVICE_ID --format=json | jq -r .status`
while [ "$STATE" != "RUNNING" ] ; do
  echo "STATE = $STATE"
  sleep 5
  STATE=`gcloud compute instances describe $DEVICE_ID --format=json | jq -r .status`
done

# Get Public IP - HACK - should look it up
IP=`gcloud compute instances describe $DEVICE_ID --format=json | jq -r .networkInterfaces[0].accessConfigs[0].natIP`
CIDR=32

ENV_VAR="\"google\": true,"

export ADMIN_IP="$IP/$CIDR"

# Make sure our key is in place.
gcloud compute ssh root@$DEVICE_ID --ssh-key-file $HOME/.ssh/id_rsa --command "date"

. ./run-in-system.sh

echo "Google Device ID: $DEVICE_ID"
echo "repeat Google run: ./run-in-google.sh --device-id=${DEVICE_ID}"
echo "SSH access: ssh -X root@${IP}"
