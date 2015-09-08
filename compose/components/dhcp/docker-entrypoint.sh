#!/bin/bash
set -e
set -x

consul agent --join consul --config-dir /etc/consul.d --data-dir /data &

answer=""
# We need consul to converge on a leader.
# This can take a little time, so we ask for
# leader status.  The leader status returns
# nothing, a string that says no leader, or
# the leader IP:port pair.  The default port
# is 8300 for server communication.
while [[ $answer != *:8300* ]]; do
  sleep 1
  answer=`curl http://localhost:8500/v1/status/leader`
  echo "Waiting for consul leader: $answer"
done

while [[ ! -e /etc/rebar-data/crowbar-key.sh ]] ; do
  echo "Waiting for crowbar-key.sh to show up"
  sleep 5
done

# Wait for the webserver to be ready.
. /etc/rebar-data/crowbar-key.sh
while ! crowbar ping; do
  sleep 1
  . /etc/rebar-data/crowbar-key.sh
done

service ssh start

# This could error if this is the first time.  Ignore it
#set +e
# Node id is harcoded here, and that is a Bad Thing
if blob="$(crowbar deployments get 1 attrib crowbar-access_keys)"; then
  mkdir -p /root/.ssh
  touch /root/.ssh/authorized_keys
  awk -F\" '{ print $4 }' <<<"$blob" >> /root/.ssh/authorized_keys
  chmod 700 /root/.ssh/authorized_keys
fi

#set -e
ip_re='([0-9a-f.:]+/[0-9]+)'
if ! [[ $(ip -4 -o addr show |grep 'scope global' |grep -v ' lo' |grep -v ' dynamic') =~ $ip_re ]]; then
  echo "Cannot find IP address for the admin node!"
  exit 1
fi
IPADDR="${BASH_REMATCH[1]}"
echo "Using $IPADDR for this host"

DOMAIN="$(crowbar nodes get "system-phantom.internal.local" attrib dns-domain | jq -r .value)"
if [ $DOMAIN == "null" ] ; then
  echo "Domain must be set to something"
  exit 1
fi

HOSTNAME=dhcp.$DOMAIN

# Add node to OpenCrowbar
if ! crowbar nodes show "$HOSTNAME"; then
  # Create a new node for us,
  # Let the annealer do its thing.
  crowbar nodes import "{\"name\": \"$HOSTNAME\", \"admin\": true, \"ip\": \"$IPADDR\", \"bootenv\": \"local\"}"|| {
    echo "We could not create a node for ourself!"
    #exit 1
  }
else
  echo "Node already created, moving on"
fi

# does the crowbar-joined-role exist?
if ! crowbar nodes roles $HOSTNAME  |grep -q 'crowbar-joined-node'; then
  crowbar nodes bind "$HOSTNAME" to 'crowbar-joined-node'
  if [ "$THE_LOCAL_NETWORK" != "" ] ; then
    crowbar nodes bind "$HOSTNAME" to "network-${THE_LOCAL_NETWORK}"
  fi
  crowbar nodes bind "$HOSTNAME" to 'dhcp-database'
  crowbar nodes commit "$HOSTNAME" || {
    echo "We could not commit the node!"
    #exit 1
  }
else
  echo "Node already committed, moving on"
fi

# Always make sure we are marking the node alive. It will comeback later.
crowbar nodes update "$HOSTNAME" "{\"alive\": true, \"bootenv\": \"local\"}"

while [ true ] ; do
  sleep 30
done
