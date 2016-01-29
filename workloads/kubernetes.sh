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
help_options["--deployment-os=<String>"]="Deployment OS: centos7 default"
help_options["--teardown"]="Turn down deployment"
help_options["--keep-admin"]="Keeps admin node running (modifies teardown)"
help_options["--wait-on-converge=true|false"]="Wait for converge of admin - defaults to true"
help_options["--dns-domain"]="Domain name to append to node names: neode.local"

help_options["--kubernetes-master-count=<Number>"]="Number of masters to start"
help_options["--kubernetes-node-count=<Number>"]="Number of nodes to start"
help_options["--kubernetes-gateway-count=<Number>"]="Number of gateway nodes to start (opencontrail only)"

help_options["--kubernetes-bin-dir=<String>"]="Directory to store binaries: /usr/local/bin"
help_options["--kubernetes-local-release-dir=<String>"]="Directory to download binaries: /tmp/releases"
help_options["--kubernetes-log-level=<Int>"]="Kubernetes Log Level: 2"
help_options["--kubernetes-users=<String>"]="JSON string of users with password and role"

help_options["--kubernetes-cluster-name=<String>"]="Name of cluster: cluster.local"
help_options["--kubernetes-kube-service-addresses=<CIDRIP>"]="Internal Service IP Addresses"

help_options["--kubernetes-network-category=<category name>"]="Network category to use for underlay traffic, default: admin"
help_options["--kubernetes-networking=<calico|flannel|opencontrail>"]="Network mode to use"

help_options["--kubernetes-pods-subnet=<CIDRIP>"]="Subnet whole calico/flannel subnet space (dotted quad)"
help_options["--kubernetes-network-prefix=<Number>"]="Subnet prefix for whole calico/flannel subnet space"
help_options["--kubernetes-network-node-prefix=<Number>"]="Subnet prefix for node calico/flannel subnet space"

help_options["--kubernetes-opencontrail-public-subnet=<CIDRIP>"]="Public network space for opencontrail"

help_options["--kubernetes-dns=<true|false>"]="Use DNS add-on"
help_options["--kubernetes-dns-upstream=<JSON ARRAY of IP>"]="JSON array of DNS server IPs"
help_options["--kubernetes-dns-replicas=<Number>"]="Number of DNS replicas to run"
help_options["--kubernetes-dns-namespace=<Kuberetenes Namespace>"]="Namespace to put the DNS service in"
help_options["--kubernetes-dns-domain=<Domain String>"]="Domain of the internal Kubernetes DNS service"

help_options["--kubernetes-ui=<true|false>"]="Use Kube-UI"
help_options["--kubernetes-dash=<true|false>"]="Use Kube-Dash"
help_options["--kubernetes-cluster-logging=<true|false>"]="Use cluster logging"
help_options["--kubernetes-cluster-monitoring=<true|false>"]="Use cluster monitoring"
help_options["--kubernetes-fabric8=<true|false>"]="Use Fabric8 console"

help_options["--kubernetes-test=<true|false>"]="Add the test role to validate completion"

help_options["--kubernetes-etcd-peer-port=<Int>"]="Default etcd peer port: 2380"
help_options["--kubernetes-etcd-client-port=<Int>"]="Default etcd client port: 2379"

# Mostly likely will require host - make it the default
DEFAULT_ACCESS=HOST

# Make sure we have the workload
WL_KUBERNETES=true

# Turn off provisioner by default
CON_NO_PROVISIONER=true

KEEP_ADMIN=false
WAIT_ON_CONVERGE=true

#
# Process config and validate providers
#
. workloads/wl-lib.sh

DNS_DOMAIN=${DNS_DOMAIN:-neode.local}

K_GATEWAY_COUNT_DEFAULT=0
KUBERNETES_NETWORKING=${KUBERNETES_NETWORKING:-flannel}
PROVIDER_GOOGLE_INSTANCE_TYPE=${PROVIDER_GOOGLE_INSTANCE_TYPE:-n1-standard-2}
PROVIDER_AWS_INSTANCE_TYPE=${PROVIDER_AWS_INSTANCE_TYPE:-m3.medium}
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
if [[ ! $ADMIN_ALREADY_UP && $WAIT_ON_CONVERGE == true ]] ; then
  if ! rebar converge ; then
    die "Admin node did NOT converge to completion"
  fi
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
rebar deployments bind $DEPLOYMENT_NAME to kubernetes-deploy

if [[ $KUBERNETES_BIN_DIR ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-bin_dir to "{ \"value\": \"${KUBERNETES_BIN_DIR}\" }"
fi
if [[ $KUBERNETES_LOCAL_RELEASE_DIR ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-local_release_dir to "{ \"value\": \"${KUBERNETES_LOCAL_RELEASE_DIR}\" }"
fi
if [[ $KUBERNETES_LOG_LEVEL ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-log_level to "{ \"value\": ${KUBERNETES_LOG_LEVEL} }"
fi
if [[ $KUBERNETES_USERS ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-users to "{ \"value\": ${KUBERNETES_USERS} }"
fi

if [[ $KUBERNETES_NETWORKING ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-networking to "{ \"value\": \"${KUBERNETES_NETWORKING}\" }"
fi
if [[ $KUBERNETES_NETWORK_CATEGORY ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-network-category to "{ \"value\": \"${KUBERNETES_NETWORK_CATEGORY}\" }"
fi

if [[ $KUBERNETES_CLUSTER_NAME ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-cluster_name to "{ \"value\": \"${KUBERNETES_CLUSTER_NAME}\" }"
fi
if [[ $KUBERNETES_KUBE_SERVICE_ADDRESSES ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-kube_service_addresses to "{ \"value\": \"${KUBERNETES_KUBE_SERVICE_ADDRESSES}\" }"
fi

if [[ $KUBERNETES_PODS_SUBNET ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-pods_subnet to "{ \"value\": \"${KUBERNETES_PODS_SUBNET}\" }"
fi
if [[ $KUBERNETES_NETWORK_PREFIX ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-network_prefix to "{ \"value\": \"${KUBERNETES_NETWORK_PREFIX}\" }"
fi
if [[ $KUBERNETES_NETWORK_NODE_PREFIX ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-network_node_prefix to "{ \"value\": \"${KUBERNETES_NETWORK_NODE_PREFIX}\" }"
fi

if [[ $KUBERNETES_OPENCONTRAIL_PUBLIC_SUBNET ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-opencontrail_public_subnet to "{ \"value\": \"${KUBERNETES_OPENCONTRAIL_PUBLIC_SUBNET}\" }"
fi

if [[ $KUBERNETES_ETCD_PEER_PORT ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-etcd_peer_port to "{ \"value\": ${KUBERNETES_ETCD_PEER_PORT} }"
fi
if [[ $KUBERNETES_ETCD_CLIENT_PORT ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-etcd_client_port to "{ \"value\": ${KUBERNETES_ETCD_CLIENT_PORT} }"
fi

if [[ $KUBERNETES_DNS == true ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-dns_setup to "{ \"value\": true }"
else
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-dns_setup to "{ \"value\": false }"
fi
if [[ $KUBERNETES_DNS_UPSTREAM ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-upstream_dns_servers to "{ \"value\": ${KUBERNETES_DNS_UPSTREAM} }"
fi
if [[ $KUBERNETES_DNS_REPLICAS ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-dns_replicas to "{ \"value\": ${KUBERNETES_DNS_REPLICAS} }"
fi
KUBERNETES_DNS_DOMAIN=${KUBERNETES_DNS_DOMAIN:-$DNS_DOMAIN}
if [[ $KUBERNETES_DNS_DOMAIN ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-dns_domain to "{ \"value\": \"${KUBERNETES_DNS_DOMAIN}\" }"
fi
if [[ $KUBERNETES_DNS_NAMESPACE ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-dns_namespace to "{ \"value\": ${KUBERNETES_DNS_NAMESPACE} }"
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
    rebar nodes bind $NAME to opencontrail-gateway
done

# Add deploy
NAME="${DEPLOYMENT_NAME}-master-0.${DNS_DOMAIN}"
rebar nodes bind $NAME to kubernetes-deploy

# Add the add-ons and tests if needed/requested
if [[ $KUBERNETES_DNS == true ]]; then
    rebar nodes bind $NAME to kubernetes-dns
fi
if [[ $KUBERNETES_UI == true ]]; then
    rebar nodes bind $NAME to kubernetes-ui
fi
if [[ $KUBERNETES_DASH == true ]]; then
    rebar nodes bind $NAME to kubernetes-dash
fi
if [[ $KUBERNETES_FABRIC8 == true ]]; then
    rebar nodes bind $NAME to kubernetes-fabric8
fi
if [[ $KUBERNETES_CLUSTER_LOGGING == true ]]; then
    rebar nodes bind $NAME to kubernetes-logging
fi
if [[ $KUBERNETES_CLUSTER_MONITORING == true ]]; then
    rebar nodes bind $NAME to kubernetes-monitoring
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

