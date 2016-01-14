#!/usr/bin/env bash

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

echo "aws:all:centos7 = $(lookup_image_id "aws" "all" "centos7")"
echo "aws:fred:centos7 = $(lookup_image_id "aws" "fred" "centos7")"
echo "aws:us-west-2:centos7 = $(lookup_image_id "aws" "us-west-2" "centos7")"
echo "packet:fred:centos7 = $(lookup_image_id "packet" "fred" "centos7")"
echo "google:jkk:centos7 = $(lookup_image_id "google" "jjk" "centos7")"


