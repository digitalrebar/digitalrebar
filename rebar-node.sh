#!/bin/bash
# Copyright 2015, Greg Althaus
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#  http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

shopt -s extglob
set -o pipefail

date

# setup & load env info
. ./bootstrap.sh

set -e
set -x

# Make sure that hostname is there
check_hostname
HOSTNAME=$(hostname)

# Main requirement for this is to have the machine key in place.
if [ ! -f /etc/rebar.install.key ] ; then
  echo "Please put rebar install key in place"
  exit 1
fi

# Make sure that the admin parm is specified
if [ "$1" == "" ] ; then
  echo "Please specify the admin node's IP address"
  exit 2
fi
admin_ip="$1"

ip_re='([0-9a-f.:]+/[0-9]+)'

# install the database
chef-solo -c /opt/digitalrebar/core/bootstrap/chef-solo.rb -o "${node_recipes}"

# Build access values
export REBAR_KEY=$(cat /etc/rebar.install.key)
export REBAR_ENDPOINT="http://$1:3000"

# This could error if this is the first time.  Ignore it
set +e
# Node id is harcoded here, and that is a Bad Thing
if blob="$(rebar nodes get 2 attrib provisioner-access_keys)"; then
    awk -F\" '{ print $4 }' <<<"$blob" >> /root/.ssh/authorized_keys
fi

set -e
if ! [[ $(ip -4 -o addr show |grep 'scope global' |grep -v ' lo' |grep -v ' dynamic') =~ $ip_re ]]; then
    echo "Cannot find IP address for the admin node!"
    exit 1
fi
IPADDR="${BASH_REMATCH[1]}"
echo "Using $IPADDR for this host"

# Add node to DigitalRebar
if ! rebar nodes show "$HOSTNAME"; then
    # Create a new node for us,
    # Let the annealer do its thing.
    rebar nodes import "{\"name\": \"$HOSTNAME\", \"admin\": true, \"ip\": \"$IPADDR\", \"bootenv\": \"local\"}"|| {
        echo "We could not create a node for ourself!"
        exit 1
    }
else
    echo "Node already created, moving on"
fi

# does the rebar-joined-role exist?
if ! rebar nodes roles $HOSTNAME  |grep -q 'rebar-joined-node'; then
    rebar nodes bind "$HOSTNAME" to 'rebar-joined-node'
    rebar nodes commit "$HOSTNAME" || {
        echo "We could not commit the node!"
        exit 1
    }
else
    echo "Node already committed, moving on"
fi

# Always make sure we are marking the node alive. It will comeback later.
rebar nodes update "$HOSTNAME" "{\"alive\": $2, \"bootenv\": \"local\"}"

