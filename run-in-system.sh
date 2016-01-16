#!/usr/bin/env bash
# Copyright 2015, RackN Inc

ACCOUNT=${ACCOUNT:-"--user root"}
LOGIN_USER=${LOGIN_USER:-root}

# Load it up
. workloads/wl-lib.sh

# If we are on mac and local host config is set.
if [[ $(uname -s) = Darwin && $DEPLOY_ADMIN = local ]] ; then
    if ! which docker &>/dev/null; then
        echo "Please install docker!"
        echo "Install Docker for MacOS by using these steps:"
        echo "  https://docs.docker.com/mac/step_one/"
        echo "Additional one-time modifications need to be made:"
cat <<EOF
Switch to NFS instead of vboxsf - one time on the mac.
sudo vi /etc/exports
/Users -maproot=root:wheel 192.168.99.100
sudo nfsd restart

From boot2docker umount/remount /Users using NFS:
Create the script in the boot2docker image: /var/lib/boot2docker/bootlocal.sh
with the contents:
# --START--
umount /Users
/usr/local/etc/init.d/nfs-client start
mount 192.168.99.1:/Users /Users -o rw,async,noatime,rsize=32768,wsize=32768,proto=tcp
# --END--

Edit the host-only network to reduce the dhcp address range to one address
 - This only needs to be done if you are going to PXE boot vms in virtualbox.

Stop the virtual machine
Up the memory in the docker vm to more - 6G
Up the cores assigned to at least 2 (better is 4).
Stop virtual box and restart it
Start headless
EOF
        ERROR="YES"
    fi

    if ! which docker-compose &>/dev/null; then
        echo "Please install docker-compose!"
        echo "Should have come with the docker tools"
        ERROR="YES"
    fi

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
  ${CON_VAR}
  ${WL_VAR}
}"

if [[ $DEPLOY_ADMIN = local ]] ; then
    LC="--connection=local"
else
    ssh-keygen -f "~/.ssh/known_hosts" -R $IP
    ssh -o BatchMode=yes -o StrictHostKeyChecking=no root@$IP date
    if [ $? -ne 0 ]; then
        echo scripts/ssh-copy-id.sh $CLEAN_IT $ID_FILE $ACCOUNT $INIT_ID_FILE $LOGIN_USER@$IP
        scripts/ssh-copy-id.sh $CLEAN_IT $ID_FILE $ACCOUNT $INIT_ID_FILE $LOGIN_USER@$IP
    fi
fi

# Build ansible inventory file
echo "$IP ansible_ssh_user=root" > /tmp/run-in-hosts.$$

export ANSIBLE_HOST_KEY_CHECKING=False
if [[ $ENV_VAR = "\"packet\": true," ]] ; then
    ansible-playbook -i /tmp/run-in-hosts.$$ --extra-vars "$JSON_STRING" tasks/packet_isos.yml
fi
ansible-playbook -i /tmp/run-in-hosts.$$ --extra-vars "$JSON_STRING" digitalrebar.yml ${LC}

echo "=== HELPFUL COMMANDS ==="
echo "repeat Ansible run: ansible-playbook -i /tmp/run-in-hosts.$$ --extra-vars \"$JSON_STRING\" digitalrebar.yml ${LC}"
echo "Digital Rebar UI https://${IP}:3000"
