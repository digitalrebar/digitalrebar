#!/usr/bin/env bash

# Copyright RackN 2016
# workloads/dev-kubernetes.sh
#

start_args="$@"

. workloads/wl-init.sh

#
# Kubernetes Config options
#
help_options["--deployment-name=<String>"]="Deployment name to hold all the nodes"
help_options["--deployment-os=<String>"]="Deployment OS: centos7 default"
help_options["--dns-domain"]="Domain name to append to node names: neode.local"

help_options["--kubernetes-master-count=<Number>"]="Number of masters to start"
help_options["--kubernetes-node-count=<Number>"]="Number of nodes to start"
help_options["--kubernetes-gateway-count=<Number>"]="Number of gateway nodes to start (opencontrail only)"

help_options["--kubernetes-bin-dir=<String>"]="Directory to store binaries: /usr/local/bin"
help_options["--kubernetes-local-release-dir=<String>"]="Directory to download binaries: /tmp/releases"
help_options["--kubernetes-log-level=<Int>"]="Kubernetes Log Level: 2"
help_options["--kubernetes-users=<String>"]="JSON string of users with password and role"

help_options["--kubernetes-cloud-provider=<String>"]="Is kubernetes in a cloud environment (false or type)"
help_options["--kubernetes-cloud-provider-type=<String>"]="Which cloud environment"

help_options["--kubernetes-cluster-name=<String>"]="Name of cluster: cluster.local"
help_options["--kubernetes-kube-service-addresses=<CIDRIP>"]="Internal Service IP Addresses"

help_options["--kubernetes-network-category=<category name>"]="Network category to use for underlay traffic, default: admin"
help_options["--kubernetes-networking=<calico|flannel|opencontrail>"]="Network mode to use"

help_options["--kubernetes-pods-subnet=<CIDRIP>"]="Subnet whole calico/flannel subnet space (dotted quad)"
help_options["--kubernetes-network-prefix=<Number>"]="Subnet prefix for whole calico/flannel subnet space"
help_options["--kubernetes-network-node-prefix=<Number>"]="Subnet prefix for node calico/flannel subnet space"

help_options["--kubernetes-opencontrail-public-subnet=<CIDRIP>"]="Public network space for opencontrail"
help_options["--kubernetes-opencontrail-private-subnet=<CIDRIP>"]="Private network space for opencontrail"
help_options["--kubernetes-opencontrail-no-arp=<true|false>"]="Should opencontrail arp or not: Google should not.  Make true for that."

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
help_options["--kubernetes-guestbook=<true|false>"]="Add the guestbook role to start a guestbook app"

help_options["--kubernetes-etcd-peer-port=<Int>"]="Default etcd peer port: 2380"
help_options["--kubernetes-etcd-client-port=<Int>"]="Default etcd client port: 2379"

# Mostly likely will require host - make it the default
DEFAULT_ACCESS=HOST

# Make sure we have the workload
WL_KUBERNETES=true

# Turn off provisioner by default
CON_NO_PROVISIONER=true

KEEP_ADMIN=true
WAIT_ON_CONVERGE=true

#
# Process config and validate providers
#
. workloads/wl-lib.sh

#
# Make sure that the domain name matches the domain that the 
# provider will create.
#
DNS_DOMAIN=${DNS_DOMAIN:-rebar.local}
KUBERNETES_NETWORKING=${KUBERNETES_NETWORKING:-flannel}
KUBERNETES_NODE_COUNT=${KUBERNETES_NODE_COUNT:-3}
DEPLOYMENT_NAME=${DEPLOYMENT_NAME:-kubs}
DEPLOYMENT_OS=${DEPLOYMENT_OS:-centos7}
REBAR_ENDPOINT=${REBAR_ENDPOINT:-"https://$ADMIN_IP:3000"}
REBAR_KEY=${REBAR_KEY:-"rebar:rebar1"}

# test Rebar
if ! rebar ping -E $REBAR_ENDPOINT; then
    die "Rebar Must Be Running: tools/docker-admin --access=HOST --no-provisioner --no-dhcp"
else
    echo "Verified Admin @ $REBAR_ENDPOINT"
fi

if [[ $TEARDOWN ]] ; then
    for ((i=1 ; i <= $KUBERNETES_NODE_COUNT; i++)) ; do
        NAME="node${i}.${DNS_DOMAIN}"
        rebar nodes destroy $NAME
        export REBAR_ENDPOINT=$REBAR_ENDPOINT && vagrant destroy -f "node${i}"
    done
    # Destroy deployment
    rebar deployments destroy "$DEPLOYMENT_NAME"
    # Clear playbooks
    docker exec -it compose_rebar_api_1 sudo rm -rf /var/cache/rebar/ansible_playbook/kubernetes-deploy/
    exit 0
fi

# Wait for the system to converge
if ! rebar converge system -E $REBAR_ENDPOINT; then
    die "Admin node $REBAR_ENDPOINT did NOT converge to completion"
else
    echo "Verified System Ready @ $REBAR_ENDPOINT"
fi

# In case this is a reset, make sure that we've removed the old deployment & playbooks to force reload
rebar deployments destroy "$DEPLOYMENT_NAME"
docker exec -it compose_rebar_api_1 sudo rm -rf /var/cache/rebar/ansible_playbook/kubernetes-deploy/

#
# Start up machines for nodes (fast if they are already running)
#
echo "Creating $KUBERNETES_NODE_COUNT nodes on $REBAR_ENDPOINT"
for ((i=1 ; i <= $KUBERNETES_NODE_COUNT; i++)) ; do
    export REBAR_ENDPOINT=$REBAR_ENDPOINT && vagrant up "node${i}"
    NODE_IP=$(rebar nodes show "node$i.${DNS_DOMAIN}" | jq --raw-output '.["node-control-address"]' | cut -d '/' -f 1)
    echo "Added Node $i at $NODE_IP"
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

if [[ $KUBERNETES_CLOUD_PROVIDER ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-cloud-provider to "{ \"value\": ${KUBERNETES_CLOUD_PROVIDER} }"
fi
if [[ $KUBERNETES_CLOUD_PROVIDER_TYPE ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-cloud-provider-type to "{ \"value\": \"${KUBERNETES_CLOUD_PROVIDER_TYPE}\" }"
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

if [[ $KUBERNETES_OPENCONTRAIL_PRIVATE_SUBNET ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-opencontrail_private_subnet to "{ \"value\": \"${KUBERNETES_OPENCONTRAIL_PRIVATE_SUBNET}\" }"
fi
if [[ $KUBERNETES_OPENCONTRAIL_NO_ARP ]]; then
    rebar deployments set $DEPLOYMENT_NAME attrib kubernetes-opencontrail_no_arp to "{ \"value\": ${KUBERNETES_OPENCONTRAIL_NO_ARP} }"
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
NAME="node1.${DNS_DOMAIN}"
rebar nodes move $NAME to $DEPLOYMENT_NAME
rebar nodes bind $NAME to kubernetes-master

# Add nodes
for ((i=2 ; i <= $KUBERNETES_NODE_COUNT; i++)) ; do
    NAME="node${i}.${DNS_DOMAIN}"
    rebar nodes move $NAME to $DEPLOYMENT_NAME
    rebar nodes bind $NAME to kubernetes-node
done

# Add deploy
NAME="node1.${DNS_DOMAIN}"
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
if [[ $KUBERNETES_GUESTBOOK == true ]]; then
    rebar nodes bind $NAME to kubernetes-guestbook
fi

# Commit it all and start
rebar deployments commit $DEPLOYMENT_NAME

echo "Access Digital Rebar UI, ${REBAR_ENDPOINT}"
echo "... config complete > converging Kubernetes ..."

# Wait for the system to converge
if ! rebar converge $DEPLOYMENT_NAME ; then
  die "Machines did NOT converge in kubernetes"
fi

if [ "$DEVICE_ID" != "" ] ; then
    EXTRA="--device-id=$DEVICE_ID"
fi

echo "Converge Compelete"

# Hint so user knows which IP to use for Master
MANAGER_IP=$(rebar nodes show "node1.${DNS_DOMAIN}" | jq --raw-output '.["node-control-address"]' | cut -d '/' -f 1)
echo "To test Kubernetes, use Master $i at https://${MANAGER_IP}/ui"
