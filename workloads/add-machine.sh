#!/usr/bin/env bash

#
# workloads/base.sh
#

start_args="$@"

#
# Process config and validate providers
#
. workloads/wl-init.sh
help_options["--node-name=<String>"] = "Name of Node"
help_options["--node-ip=<IP>"] = "IP to use for system provider
help_options["--node-os=<String>"] = "OS to use for non-system provider"
NODE_OS=centos7
NODE_NAME="node-$(date "+%H%M%S")"
. workloads/wl-lib.sh

if [[ ! $ADMIN_IP ]] ; then
    echo "Must specify --admin-ip"
    exit 1
fi

if [[ $PROVIDER == system && ! $NODE_IP ]] ; then
    echo "Must specify --node-ip"
    exit 1
fi

if [[ $PROVIDER != system && $NODE_IP ]] ; then
   echo "Don't specify a --node-ip with a non-system provider"
   exit 1
fi

start_machine $NODE_NAME $NODE_IP $NODE_OS

