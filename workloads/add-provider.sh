#!/usr/bin/env bash

#
# workloads/base.sh
#

start_args="$@"

#
# Process config and validate providers
#
. workloads/wl-lib.sh

if [[ ! $ADMIN_IP ]] ; then
    echo "Must specify --admin-ip"
    exit 1
fi

validate_provider $PROVIDER
add_provider

