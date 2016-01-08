#!/usr/bin/env bash

#
# workloads/kubernetes.sh
#

start_args="$@"

. workloads/wl-init.sh

#
# Kubernetes Config options
#
help_options["--deployment-name=<String>"]="Deployment name to hold all the nodes"
help_options["--teardown"]="Turn down deployment"
help_options["--keep-admin"]="Keeps admin node running (modifies teardown)"
help_options["--dns-domain"]="Domain name to append to node names: neode.local"

help_options["--kubernetes-master-count=<Number>"]="Number of masters to start"
help_options["--kubernetes-node-count=<Number>"]="Number of nodes to start"
help_options["--kubernetes-gateway-count=<Number>"]="Number of gateway nodes to start (opencontrail only)"

help_options["--kubernetes-source-type=<packageManager|localBuild>"]="Where to get kubernetes from"
help_options["--kubernetes-cluster-name=<String>"]="Name of cluster: cluster.local"
help_options["--kubernetes-kube-service-addresses=<CIDRIP>"]="Internal Service IP Addresses"

help_options["--kubernetes-networking=<flannel|opencontrail>"]="Network mode to use"
help_options["--kubernetes-network-category=<category name>"]="Network category to use for underlay traffic, default: admin"

help_options["--kubernetes-flannel-subnet=<IP>"]="Subnet whole flannel subnet space (dotted quad)"
help_options["--kubernetes-flannel-prefix=<Number>"]="Subnet prefix for whole flannel subnet space"
help_options["--kubernetes-flannel-host-prefix=<Number>"]="Subnet prefix for host section flannel subnet space"

help_options["--kubernetes-opencontrail-public-subnet=<CIDRIP>"]="Public network space for opencontrail"
help_options["--kubernetes-opencontrail-private-subnet=<CIDRIP>"]="Private network space for opencontrail"

help_options["--kubernetes-dns=<true|false>"]="Use DNS add-on"
help_options["--kubernetes-dns-replicas=<Number>"]="Number of DNS replicas to run"
help_options["--kubernetes-ui=<true|false>"]="Use Kube-UI"
help_options["--kubernetes-cluster-logging=<true|false>"]="Use cluster logging"
help_options["--kubernetes-cluster-monitoring=<true|false>"]="Use cluster monitoring"

help_options["--kubernetes-test=<true|false>"]="Add the test role to validate completion"


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

DNS_DOMAIN=${DNS_DOMAIN:-neode.local}

K_GATEWAY_COUNT_DEFAULT=0
KUBERNETES_NETWORKING=${KUBERNETES_NETWORKING:-flannel}
if [[ $KUBERNETES_NETWORKING == opencontrail ]]; then
    K_GATEWAY_COUNT_DEFAULT=1
fi

KUBERNETES_MASTER_COUNT=${KUBERNETES_MASTER_COUNT:-1}
KUBERNETES_NODE_COUNT=${KUBERNETES_NODE_COUNT:-3}
KUBERNETES_GATEWAY_COUNT=${KUBERNETES_GATEWAY_COUNT:-$K_GATEWAY_COUNT_DEFAULT}
DEPLOYMENT_NAME=${DEPLOYMENT_NAME:-kubs}
DEPLOYMENT_OS=${DEPLOYMENT_OS:-centos7}

# GREG: VALIDATE PARMS
# networking_mode == opencontrail|flannel
# if opencontrail -> Gateway >= 1
# if flannel -> Gateway = 0
# Must have 1 master
# Must have 1 node

bring_up_admin "${DEPLOYMENT_NAME}-admin.${DNS_DOMAIN}"

if [[ $TEARDOWN ]] ; then
    for ((i=0 ; i < $KUBERNETES_GATEWAY_COUNT; i++)) ; do
        NAME="${DEPLOYMENT_NAME}-gateway-$i.${DNS_DOMAIN}"
        rebar nodes destroy $NAME
    done
    for ((i=0 ; i < $KUBERNETES_NODE_COUNT; i++)) ; do
        NAME="${DEPLOYMENT_NAME}-node-$i.${DNS_DOMAIN}"
        rebar nodes destroy $NAME
    done
    for ((i=0 ; i < $KUBERNETES_MASTER_COUNT; i++)) ; do
        NAME="${DEPLOYMENT_NAME}-master-$i.${DNS_DOMAIN}"
        rebar nodes destroy $NAME
    done

    # Destroy deployment
    rebar deployments destroy "$DEPLOYMENT_NAME"

    if [[ $KEEP_ADMIN == false ]] ; then
        tear_down_admin "${DEPLOYMENT_NAME}-admin.${DNS_DOMAIN}"
    fi

    exit 0
fi

# Wait for the system to converge
if ! rebar converge ; then
  die "Admin node did NOT converge to completion"
fi

add_provider

#
# Start up machines for masters
#
for ((i=0 ; i < $KUBERNETES_MASTER_COUNT; i++)) ; do
    NAME="${DEPLOYMENT_NAME}-master-$i.${DNS_DOMAIN}"
    start_machine $NAME $DEPLOYMENT_OS
done

#
# Start up machines for nodes
#
for ((i=0 ; i < $KUBERNETES_NODE_COUNT; i++)) ; do
    NAME="${DEPLOYMENT_NAME}-node-$i.${DNS_DOMAIN}"
    start_machine $NAME $DEPLOYMENT_OS
done

#
# Start up machines for gateways
#
for ((i=0 ; i < $KUBERNETES_GATEWAY_COUNT; i++)) ; do
    NAME="${DEPLOYMENT_NAME}-gateway-$i.${DNS_DOMAIN}"
    start_machine $NAME $DEPLOYMENT_OS
done

# Create deployment
rebar deployments create "{ \"name\": \"$DEPLOYMENT_NAME\" }"

# Configure default parameters
rebar deployments bind $DEPLOYMENT_NAME to kubernetes-config

if [[ $KUBERNETES_NETWORKING ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-networking to "{ \"value\": \"${KUBERNETES_NETWORKING}\" }"
fi
if [[ $KUBERNETES_NETWORK_CATEGORY ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-network-category to "{ \"value\": \"${KUBERNETES_NETWORK_CATEGORY}\" }"
fi

if [[ $KUBERNETES_SOURCE_TYPE ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-source_type to "{ \"value\": \"${KUBERNETES_SOURCE_TYPE}\" }"
fi
if [[ $KUBERNETES_CLUSTER_NAME ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-cluster_name to "{ \"value\": \"${KUBERNETES_CLUSTER_NAME}\" }"
fi
if [[ $KUBERNETES_KUBE_SERVICE_ADDRESSES ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-kube_service_addresses to "{ \"value\": \"${KUBERNETES_KUBE_SERVICE_ADDRESSES}\" }"
fi

if [[ $KUBERNETES_FLANNEL_SUBNET ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-flannel_subnet to "{ \"value\": \"${KUBERNETES_FLANNEL_SUBNET}\" }"
fi
if [[ $KUBERNETES_FLANNEL_PREFIX ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-flannel_prefix to "{ \"value\": \"${KUBERNETES_FLANNEL_PREFIX}\" }"
fi
if [[ $KUBERNETES_FLANNEL_HOST_PREFIX ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-flannel_host_prefix to "{ \"value\": \"${KUBERNETES_FLANNEL_HOST_PREFIX}\" }"
fi

if [[ $KUBERNETES_OPENCONTRAIL_PUBLIC_SUBNET ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-opencontrail_public_subnet to "{ \"value\": \"${KUBERNETES_OPENCONTRAIL_PUBLIC_SUBNET}\" }"
fi
if [[ $KUBERNETES_OPENCONTRAIL_PRIVATE_SUBNET ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-opencontrail_private_subnet to "{ \"value\": \"${KUBERNETES_OPENCONTRAIL_PRIVATE_SUBNET}\" }"
fi

KUBERNETES_ADDONS=false

if [[ $KUBERNETES_DNS ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-dns_setup to "{ \"value\": ${KUBERNETES_DNS} }"
    KUBERNETES_ADDONS=true
fi
if [[ $KUBERNETES_DNS_REPLICAS ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-dns_replicas to "{ \"value\": ${KUBERNETES_DNS_REPLICAS} }"
fi
if [[ $KUBERNETES_UI ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-kube-ui to "{ \"value\": ${KUBERNETES_UI} }"
    KUBERNETES_ADDONS=true
fi
if [[ $KUBERNETES_CLUSTER_LOGGING ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-cluster_logging to "{ \"value\": ${KUBERNETES_CLUSTER_LOGGING} }"
    KUBERNETES_ADDONS=true
fi
if [[ $KUBERNETES_CLUSTER_MONITORING ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-cluster_monitoring to "{ \"value\": ${KUBERNETES_CLUSTER_MONITORING} }"
    KUBERNETES_ADDONS=true
fi

rebar deployments commit $DEPLOYMENT_NAME


# Add masters
for ((i=0 ; i < $KUBERNETES_MASTER_COUNT; i++)) ; do
    NAME="${DEPLOYMENT_NAME}-master-$i.${DNS_DOMAIN}"
    rebar nodes move $NAME to $DEPLOYMENT_NAME
    rebar nodes bind $NAME to kubernetes-master
done

# Add nodes
for ((i=0 ; i < $KUBERNETES_NODE_COUNT; i++)) ; do
    NAME="${DEPLOYMENT_NAME}-node-$i.${DNS_DOMAIN}"
    rebar nodes move $NAME to $DEPLOYMENT_NAME
    rebar nodes bind $NAME to kubernetes-node
done

# Add gateways
for ((i=0 ; i < $KUBERNETES_GATEWAY_COUNT; i++)) ; do
    NAME="${DEPLOYMENT_NAME}-gateway-$i.${DNS_DOMAIN}"
    rebar nodes move $NAME to $DEPLOYMENT_NAME
    rebar nodes bind $NAME to kubernetes-gateway
done

# Add the add-ons and tests if needed/requested
NAME="${DEPLOYMENT_NAME}-master-0.${DNS_DOMAIN}"
if [[ $KUBERNETES_ADDONS == true ]]; then
    rebar nodes bind $NAME to kubernetes-add-ons
fi

if [[ $KUBERNETES_TEST == true ]]; then
    rebar nodes bind $NAME to kubernetes-test
fi

# Commit it all and start
rebar deployments commit $DEPLOYMENT_NAME

# Wait for the system to converge
if ! rebar converge ; then
  die "Machines did NOT converge in kubernetes"
fi

if [ "$DEVICE_ID" != "" ] ; then
    EXTRA="--device-id=$DEVICE_ID"
fi
echo "To teardown, $0 $start_args --teardown=true --admin-ip=$ADMIN_IP $EXTRA"
echo "To keep the admin node, add --keep_admin=true"

