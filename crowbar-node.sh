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

date

# setup & load env info
. ./bootstrap.sh

# Make sure that hostname is there
check_hostname
HOSTNAME=$(hostname)

# Main requirement for this is to have the machine key in place.
if [ ! -f /etc/crowbar.install.key ] ; then
  echo "Please put crowbar install key in place"
  exit 1
fi

# Make sure that the admin parm is specified
if [ "$1" == "" ] ; then
  echo "Please specify the admin node's IP address"
  exit 2
fi
admin_ip="$1"

# install the database
chef-solo -c /opt/opencrowbar/core/bootstrap/chef-solo.rb -o "${node_recipes}"

# Build access values
export CROWBAR_KEY=`cat /etc/crowbar.install.key`
export CROWBAR_WEB="http://$1:3000"

# This could error if this is the first time.  Ignore it
set +e

# Get ssh keys public keys
ROLE_ID=`crowbar --hostname $admin_ip roles show provisioner-server | grep '"id"'`
ROLE_ID=${ROLE_ID##*:}
ROLE_ID=${ROLE_ID%,}
# XXX: Node id is hard-coded and bad.
NODE_ROLE_ID=`crowbar --hostname $admin_ip noderoles list | grep -B2 -A2 "\"role_id\":$ROLE_ID" | grep -B3 -A2 '"node_id": 2' | grep \"id\"`
NODE_ROLE_ID=${NODE_ROLE_ID##*:}
NODE_ROLE_ID=${NODE_ROLE_ID%,}
if [ "$NODE_ROLE_ID" != "" ] ; then
  crowbar --hostname $admin_ip noderoles get $NODE_ROLE_ID attrib provisioner-access_keys | awk -F\" '{ print $4 }' >> /root/.ssh/authorized_keys
fi

set -e

IPADDR=`ip addr show | grep "inet " | grep -v " lo" | awk '{ print $2 }'`
echo "Using $IPADDR for this host"

# Add node to OpenCrowbar
exists=$(curl -s -o /dev/null -w "%{http_code}" --digest -u "$CROWBAR_KEY" \
  -X GET "$CROWBAR_WEB/api/v2/nodes/$HOSTNAME")
if [[ $exists == 404 ]]; then
    # Create a new node for us,
    # Let the annealer do its thing.
    curl -f -g --digest -u "$CROWBAR_KEY" -X POST \
      -d "name=$HOSTNAME" \
      -d 'admin=true' \
      -d "ip=$IPADDR" \
      -d 'bootenv=local' \
      "$CROWBAR_WEB/api/v2/nodes/" || {
        echo "We could not create a node for ourself!"
        exit 1
    }
else
    echo "Node already created, moving on"
fi

# does the crowbar-joined-role exist?
managed=$(curl -s -o /dev/null -w "%{http_code}" --digest -u "$CROWBAR_KEY" \
  -X GET "$CROWBAR_WEB/api/v2/nodes/$HOSTNAME/node_roles/crowbar-joined-node")
if [[ $managed == 404 ]]; then
    curl -f -g --digest -u "$CROWBAR_KEY" -X POST \
      -d "node=$HOSTNAME" \
      -d "role=crowbar-joined-node" \
      "$CROWBAR_WEB/api/v2/node_roles/" && \
    curl -f -g --digest -u "$CROWBAR_KEY" -X PUT \
      "$CROWBAR_WEB/api/v2/nodes/$HOSTNAME/commit" || {
        echo "We could not commit the node!"
        exit 1
    }
else
    echo "Node already committed, moving on"
fi

# Always make sure we are marking the node not alive. It will comeback later.
curl -f -g --digest -u "$CROWBAR_KEY" \
    -X PUT "$CROWBAR_WEB/api/v2/nodes/$HOSTNAME" \
    -d 'alive=true' \
    -d 'bootenv=local'

