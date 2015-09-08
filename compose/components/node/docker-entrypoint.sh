#!/bin/bash
#set -e
set -x

while [[ ! -e /etc/rebar-data/crowbar-key.sh ]] ; do
  echo "Waiting for rebar-key.sh to show up"
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

HOSTNAME=$(hostname).$DOMAIN

# Let the other nodes come up
# GREG: wait for it - check instead
sleep 120

# Add node to DigitalRebar
if ! crowbar nodes show "$HOSTNAME"; then
  # Create a new node for us,
  # Let the annealer do its thing.
  crowbar nodes import "{\"name\": \"$HOSTNAME\", \"admin\": false, \"ip\": \"$IPADDR\", \"bootenv\": \"local\"}"|| {
    echo "We could not create a node for ourself!"
    #exit 1
  }
else
  echo "Node already created, moving on"
fi

# does the crowbar-joined-role exist?
if ! crowbar nodes roles $HOSTNAME  |grep -q 'crowbar-joined-node'; then
  crowbar nodes bind "$HOSTNAME" to 'crowbar-joined-node'
  crowbar nodes commit "$HOSTNAME" || {
    echo "We could not commit the node!"
    #exit 1
  }
else
  echo "Node already committed, moving on"
fi

# Always make sure we are marking the node alive. It will comeback later.
crowbar nodes update "$HOSTNAME" "{\"alive\": true, \"bootenv\": \"local\"}"

tail -f /var/log/*
