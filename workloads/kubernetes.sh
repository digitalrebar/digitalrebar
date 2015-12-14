#!/usr/bin/env bash

#
# workloads/kubernetes.sh
#

start_args="$@"

. workloads/wl-init.sh

#
# Kubernetes Config options
#
help_options["--k8s-master-count=<Number>"]="Number of masters to start"
help_options["--k8s-minion-count=<Number>"]="Number of minions to start"
help_options["--deployment-name=<String>"]="Deployment name to hold all the nodes"
help_options["--teardown"]="Turn down deployment"
help_options["--keep-admin"]="Keeps admin node running (modifies teardown)"

# Mostly likely will require host - make it the default
DEFAULT_ACCESS=HOST

# Make sure we have the workload
WL_KUBERNETES=true

# Turn off provisioner by default
CON_NO_PROVISIONER=true

KEEP_ADMIN=false

#
# Process config and validate providers
#
. workloads/wl-lib.sh

K8S_MASTER_COUNT=${K8S_MASTER_COUNT:-1}
K8S_MINION_COUNT=${K8S_MINION_COUNT:-3}
DEPLOYMENT_NAME=${DEPLOYMENT_NAME:-kubs}
DEPLOYMENT_OS=${DEPLOYMENT_OS:-centos7}

bring_up_admin "${DEPLOYMENT_NAME}-admin.neode.local"

if [[ $TEARDOWN ]] ; then
    for ((i=0 ; i < $K8S_MINION_COUNT; i++)) ; do
        NAME="${DEPLOYMENT_NAME}-minion-$i.neode.local"
        $REBAR nodes destroy $NAME
    done
    for ((i=0 ; i < $K8S_MASTER_COUNT; i++)) ; do
        NAME="${DEPLOYMENT_NAME}-master-$i.neode.local"
        $REBAR nodes destroy $NAME
    done

    # Destroy deployment
    $REBAR deployments destroy "$DEPLOYMENT_NAME"

    if [[ $KEEP_ADMIN == false ]] ; then
        tear_down_admin "${DEPLOYMENT_NAME}-admin.neode.local"
    fi

    exit 0
fi

# Wait for the system to converge
if ! $REBAR converge ; then
  die "Admin node did NOT converge to completion"
fi

add_provider

#
# Start up machines for masters
#
for ((i=0 ; i < $K8S_MASTER_COUNT; i++)) ; do
    NAME="${DEPLOYMENT_NAME}-master-$i.neode.local"
    start_machine $NAME $DEPLOYMENT_OS
done

#
# Start up machines for minions
#
for ((i=0 ; i < $K8S_MINION_COUNT; i++)) ; do
    NAME="${DEPLOYMENT_NAME}-minion-$i.neode.local"
    start_machine $NAME $DEPLOYMENT_OS
done

# Create deployment
$REBAR deployments create "{ \"name\": \"$DEPLOYMENT_NAME\" }"

# Add masters
for ((i=0 ; i < $K8S_MASTER_COUNT; i++)) ; do
    NAME="${DEPLOYMENT_NAME}-master-$i.neode.local"
    $REBAR nodes move $NAME to $DEPLOYMENT_NAME
    $REBAR nodes bind $NAME to kubernetes-master
done

# Add minions
for ((i=0 ; i < $K8S_MINION_COUNT; i++)) ; do
    NAME="${DEPLOYMENT_NAME}-minion-$i.neode.local"
    $REBAR nodes move $NAME to $DEPLOYMENT_NAME
    $REBAR nodes bind $NAME to kubernetes-minion
done

# GREG: Set parameters 

# Commit it all and start
$REBAR deployments commit $DEPLOYMENT_NAME

# Wait for the system to converge
if ! $REBAR converge ; then
  die "Machines did NOT converge in kubernetes"
fi

if [ "$DEVICE_ID" != "" ] ; then
    EXTRA="--device-id=$DEVICE_ID"
fi
echo "To teardown, $0 $start_args --teardown=true --admin-ip=$ADMIN_IP $EXTRA"
echo "To keep the admin node, add --keep_admin=true"

