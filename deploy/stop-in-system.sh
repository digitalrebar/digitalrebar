#!/usr/bin/env bash
# Copyright 2015, RackN Inc

# Load it up
. workloads/wl-lib.sh

# If we are on mac and local host config is set.
if [[ ($(uname -s) = "MINGW64_NT-10.0" || $(uname -s) = Darwin) && $DEPLOY_ADMIN = local ]] ; then
    if [ "$DOCKER_HOST" == "" ] ; then
        echo "DOCKER_HOST should be set"
        echo "Make sure you are running from a docker-enabled terminal"
        ERROR="YES"
    fi

    if [ "$ERROR" == "YES" ] ; then
        exit 1
    fi

    IP=${DOCKER_HOST%:*}
    IP=${IP##*/}

    EXTRA_VARS=""
    SUBNETISH=${IP%.*}
    netmask=$(ifconfig -a | grep $SUBNETISH | awk '{ print $4}')
    CIDR=??
    if [ "$netmask" == "0xffffff00" ] ; then
      CIDR=24
    fi
    if [ "$netmask" == "0xffff0000" ] ; then
      CIDR=16
    fi
    export ADMIN_IP="$IP/$CIDR"
fi

if [[ ! $ADMIN_IP ]] ; then
    echo "Please specify an ADMIN IP"
    exit 1
fi

CIDRIP=$ADMIN_IP
IP=${CIDRIP%/*}
CIDR=${CIDRIP##*/}
if [ "$CIDRIP" == "" ] ; then
    echo "Please provide a CIDR IP"
    exit 1
fi

echo "Device IP = $IP/$CIDR"

if [[ $ACCESS = HOST ]] ; then
  ACCESS_VAR="\"dr_access_mode\": \"HOST\", \"dr_external_ip\": \"$IP/$CIDR\","
fi

if [[ $DR_TAG ]] ; then
    TAG_VARS="\"dr_tag\": \"$DR_TAG\","
fi

CON_VAR="\"dr_services\": ["
COMMA=""
for c in "${!containers[@]}"; do
    [[ ${containers["$c"]} && ${containers["$c"]} != false ]] || continue
    CON_VAR="$CON_VAR $COMMA \"--$c\""
    COMMA=","
done
CON_VAR="${CON_VAR} ],"

WL_VAR="\"dr_workloads\": ["
COMMA=""
for c in "${!workloads[@]}"; do
    [[ ${workloads["$c"]} && ${workloads["$c"]} != false ]] || continue
    WL_VAR="$WL_VAR $COMMA \"$c\""
    COMMA=","
done
WL_VAR="${WL_VAR} ]"

JSON_STRING="{
  ${ENV_VAR}
  ${ACCESS_VAR}
  ${TAG_VARS}
  ${CON_VAR}
  ${WL_VAR}
}"

if [[ $DEPLOY_ADMIN = local ]] ; then
    LC="--connection=local"
else
    ssh-keygen -f "~/.ssh/known_hosts" -R $IP
    ssh -o BatchMode=yes -o StrictHostKeyChecking=no root@$IP date
    if [ $? -ne 0 ]; then
        echo scripts/ssh-copy-id.sh $CLEAN_IT $ID_FILE $ACCOUNT $INIT_ID_FILE root@$IP
        scripts/ssh-copy-id.sh $CLEAN_IT $ID_FILE $ACCOUNT $INIT_ID_FILE root@$IP
    fi
fi

# Build ansible inventory file
echo "$IP ansible_ssh_user=root" > /tmp/run-in-hosts.$$

export ANSIBLE_HOST_KEY_CHECKING=False
ansible-playbook -i /tmp/run-in-hosts.$$ --extra-vars "$JSON_STRING" -t stop digitalrebar.yml ${LC}

