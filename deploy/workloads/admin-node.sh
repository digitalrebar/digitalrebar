#!/usr/bin/env bash

#
# workloads/admin-node.sh
#

start_args="$@"

. workloads/wl-init.sh

#
# Add Help Here
#
#help_options["--your-var-here=<Number>"]="Number of var heres"
help_options["--teardown"]="Turn down deployment"
help_options["--keep-admin"]="Keeps admin node running (modifies teardown)"
help_options["--wait-on-converge=true|false"]="Wait for converge of admin - defaults to true"
help_options["--admin-name=<String>"]="Name of Admin node - defaults to admin.neode.local"

# Default get rid of admin node
KEEP_ADMIN=false
WAIT_ON_CONVERGE=true
ADMIN_NAME=admin.neode.local

#
# Process config and validate providers
#
. workloads/wl-lib.sh

bring_up_admin $ADMIN_NAME

if [[ $TEARDOWN ]] ; then
    if [[ $KEEP_ADMIN == false ]] ; then
        tear_down_admin "admin.neode.local"
    fi

    exit 0
fi

# Wait for the system to converge
if [[ $WAIT_ON_CONVERGE == true ]] ; then
    if ! rebar converge ; then
        die "Admin node did NOT converge to completion"
    fi
fi

if [ "$DEVICE_ID" != "" ] ; then
    EXTRA="--device-id=$DEVICE_ID"
fi
echo "To teardown, $0 $start_args --teardown=true --admin-ip=$ADMIN_IP $EXTRA"
echo "To keep the admin node, add --keep_admin=true"

