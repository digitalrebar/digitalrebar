#!/bin/bash

# Set and reverse ips.
MY_DEV=eth0
echo "Switch IP Address ordering on $MY_DEV"
MY_IP=$(ip addr show $MY_DEV | grep inet | grep -v inet6 | grep -v $FORWARDER_IP | awk '{ print $2 }')
ip addr add $FORWARDER_IP dev $MY_DEV
ip addr del $MY_IP dev $MY_DEV
ip addr add $MY_IP dev $MY_DEV
