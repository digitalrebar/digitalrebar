#!/usr/bin/env bash

#
# workloads/docker-swarm.sh
#

start_args="$@"

. workloads/wl-init.sh

#
# Docker Swarm Config options
#
help_options["--docker-swarm-manager-count=<Number>"]="Number of managers to start"
help_options["--docker-swarm-member-count=<Number>"]="Number of members to start"
help_options["--deployment-name=<String>"]="Deployment name to hold all the nodes"

# Mostly likely will require host - make it the default
DEFAULT_ACCESS=HOST

# Make sure we have the workload
WL_DOCKER_SWARM=true

# Turn off provisioner by default
CON_NO_PROVISIONER=true

KEEP_ADMIN=false

#
# Process config and validate providers
#
. workloads/wl-lib.sh

DOCKER_SWARM_MANAGER_COUNT=${DOCKER_SWARM_MANAGER_COUNT:-1}
DOCKER_SWARM_MEMBER_COUNT=${DOCKER_SWARM_MEMBER_COUNT:-3}
DEPLOYMENT_NAME=${DEPLOYMENT_NAME:-docker-swarm}
DEPLOYMENT_OS=${DEPLOYMENT_OS:-centos7}

bring_up_admin "${DEPLOYMENT_NAME}-admin.neode.local"

if [[ $TEARDOWN ]] ; then
    for ((i=0 ; i < $DOCKER_SWARM_MEMBER_COUNT; i++)) ; do
        NAME="${DEPLOYMENT_NAME}-member-$i.neode.local"
        $REBAR nodes destroy $NAME
    done
    for ((i=0 ; i < $DOCKER_SWARM_MANAGER_COUNT; i++)) ; do
        NAME="${DEPLOYMENT_NAME}-manager-$i.neode.local"
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
# Start up machines for manager
#
for ((i=0 ; i < $DOCKER_SWARM_MANAGER_COUNT; i++)) ; do
    NAME="${DEPLOYMENT_NAME}-manager-$i.neode.local"
    start_machine $NAME $DEPLOYMENT_OS
done

#
# Start up machines for members
#
for ((i=0 ; i < $DOCKER_SWARM_MEMBER_COUNT; i++)) ; do
    NAME="${DEPLOYMENT_NAME}-member-$i.neode.local"
    start_machine $NAME $DEPLOYMENT_OS
done

# Create deployment
$REBAR deployments create "{ \"name\": \"$DEPLOYMENT_NAME\" }"

# Add manager
for ((i=0 ; i < $DOCKER_SWARM_MANAGER_COUNT; i++)) ; do
    NAME="${DEPLOYMENT_NAME}-manager-$i.neode.local"
    $REBAR nodes move $NAME to $DEPLOYMENT_NAME
    $REBAR nodes bind $NAME to docker-swarm-manager
done

# Add members
for ((i=0 ; i < $DOCKER_SWARM_MEMBER_COUNT; i++)) ; do
    NAME="${DEPLOYMENT_NAME}-member-$i.neode.local"
    $REBAR nodes move $NAME to $DEPLOYMENT_NAME
    $REBAR nodes bind $NAME to docker-swarm-member
done

# GREG: Set parameters 

# Commit it all and start
$REBAR deployments commit $DEPLOYMENT_NAME

# Wait for the system to converge
if ! $REBAR converge ; then
  die "Machines did NOT converge in docker-swarm"
fi

if [ "$DEVICE_ID" != "" ] ; then
    EXTRA="--device-id=$DEVICE_ID"
fi
echo "To teardown, $0 $start_args --teardown=true --admin-ip=$ADMIN_IP $EXTRA"
echo "To keep the admin node, add --keep_admin=true"

