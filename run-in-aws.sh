#!/usr/bin/env bash
# Copyright 2015, RackN Inc

#
# We are deploying in aws add this aws instance as a provider
# and force the admin deploy to aws
#
FORCE_PROVIDER=${PROVIDER:-aws}
FORCE_DEPLOY_ADMIN=${DEPLOY_ADMIN:-aws}

# Processes args, inits provider, and validates provider
. workloads/wl-lib.sh

# Choose aws image
IID=$(lookup_image_id "aws" $PROVIDER_AWS_REGION "centos7")

# Access controls for the above image
INIT_ID_FILE="--init-ident $HOME/.ssh/$(hostname)"
CLEAN_IT="--clean"
LOGIN_USER="centos"

# Check to see if device id exists.
if [ "$DEVICE_ID" != "" ] ; then
    STATE=`aws ec2 describe-instances --instance-id $DEVICE_ID | jq -r .Reservations[0].Instances[0].State.Name`

    if [[ $STATE == null ]] ; then
        echo "Instance ID doesn't exist in aws: $DEVICE_ID"
        exit 1
    fi
    echo "AWS reuse ${DEVICE_ID}"
else
    echo "AWS will create ${NODENAME}"

    # Make name for unamed items
    NODENAME=$1
    if [ "$NODENAME" == "" ] ; then
        TSTAMP=`date +%H%M`
        NODENAME="${USER}1-${TSTAMP}"
    else
        shift
    fi

    KEY_NAME=$(hostname)
    if ! aws ec2 describe-key-pairs --key-names $(hostname) 2>/dev/null >/dev/null ; then
      aws ec2 create-key-pair --key-name $(hostname) | jq -r .KeyMaterial > ~/.ssh/$(hostname)
      chmod 600 ~/.ssh/$(hostname)
    fi

    SG_ID=$(aws ec2 describe-security-groups --group-names "digital rebar" | jq -r .SecurityGroups[0].GroupId)
    if [[ ! $SG_ID ]] ; then
        echo "Please create a security group called: 'digital rebar'"
        echo "The group should allow ICMP ping, TCP ports 22, 443, 3000"
        exit -1
    fi
  
    DEVICE_ID=`aws ec2 run-instances --image-id $IID --count 1 --instance-type m4.large --key-name $KEY_NAME --security-group-ids $SG_ID | jq -r .Instances[0].InstanceId`

    # Set Name
    aws ec2 create-tags --resources $DEVICE_ID --tags Key=Name,Value=${NODENAME%%.*}

fi

# Wait for device to be up
STATE=`aws ec2 describe-instances --instance-id $DEVICE_ID | jq -r .Reservations[0].Instances[0].State.Name`
while [ "$STATE" != "running" ] ; do
  echo "STATE = $STATE"
  sleep 5
  STATE=`aws ec2 describe-instances --instance-id $DEVICE_ID | jq -r .Reservations[0].Instances[0].State.Name`
done

# Get Public IP - HACK - should look it up
IP=`aws ec2 describe-instances --instance-id $DEVICE_ID | jq -r .Reservations[0].Instances[0].PublicIpAddress`
CIDR=32

ENV_VAR="\"aws\": true,"

export ADMIN_IP="$IP/$CIDR"

. ./run-in-system.sh

echo "AWS Device ID: $DEVICE_ID"
echo "repeat AWS run: ./run-in-aws.sh --device-id=${DEVICE_ID}"
echo "SSH access: ssh -X root@${IP}"
