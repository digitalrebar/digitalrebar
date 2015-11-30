#!/bin/bash
# Copyright 2015, RackN Inc

ERROR=""
if ! which ansible &>/dev/null; then
    echo "Please install Ansible!"
    echo "Something like: brew install ansible"
    ERROR="YES"
fi

if ! which docker &>/dev/null; then
    echo "Please install docker!"
    echo "Install Docker for MacOS by using these steps:"
    echo "  https://docs.docker.com/mac/step_one/"
    echo "Additional one-time modifications need to be made:"
cat <<EOF
Switch to NFS instead of vboxsf - one time on the mac.
vi /etc/exports # as root:
/Users -maproot=root:wheel 192.168.99.100
nfsd restart

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

if ! which jq &>/dev/null; then
    echo "Please install jq!"
    echo "Something like: brew install jq"
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

# Default to the one that will always work.
HOST_MODE="YES"
if [ "$1" == "--no-host" ] ; then
  shift
  HOST_MODE=""
fi

IP=${DOCKER_HOST%:*}
IP=${IP##*/}

EXTRA_VARS=""
if [ "$HOST_MODE" == "YES" ] ; then
  SUBNETISH=${IP%.*}
  netmask=$(ifconfig -a | grep $SUBNETISH | awk '{ print $4}')
  CIDR=??
  if [ "$netmask" == "0xffffff00" ] ; then
    CIDR=24
  fi
  if [ "$netmask" == "0xffff0000" ] ; then
    CIDR=16
  fi
  EXTRA_VARS="dr_access_mode=HOST dr_external_ip=$IP/$CIDR"
fi

echo
echo "This may ask for admin/root password to remove rebar-key."
echo "This is early in the process."
echo

echo "127.0.0.1" > /tmp/run-in-hosts.$$
export ANSIBLE_HOST_KEY_CHECKING=False
ansible-playbook -i /tmp/run-in-hosts.$$ --extra-vars "$EXTRA_VARS" digitalrebar.yml --connection=local

echo "=== HELPFUL COMMANDS ==="
echo "repeat Ansible run: ansible-playbook -i /tmp/run-in-hosts.$$ --extra-vars \"$EXTRA_VARS\" digitalrebar.yml --connection=local"
echo "Digital Rebar UI https://${IP}:3000"
