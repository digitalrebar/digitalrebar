#!/bin/bash

service ssh start

# This could error if this is the first time.  Ignore it
#set +e
# Node id is harcoded here, and that is a Bad Thing
mkdir -p /root/.ssh
touch /root/.ssh/authorized_keys
chmod 600 /root/.ssh/authorized_keys
while read line; do
    fgrep -q "$line" "/root/.ssh/authorized_keys" && continue
    echo "$line" >> "/root/.ssh/authorized_keys"
done < <(rebar deployments get 1 attrib rebar-access_keys |jq -r -c '.value | .[]')

DOMAIN="$(rebar nodes get "system-phantom.internal.local" attrib dns-domain | jq -r .value)"
if [[ $DOMAIN == null ]] ; then
  echo "Domain must be set to something"
  exit 1
fi

if [[ ! $SERVICE_NAME ]]; then
    echo "Service name not set!"
    exit 1
fi

export HOSTNAME="$SERVICE_NAME.$DOMAIN"

# Add node to DigitalRebar
if ! rebar nodes show "$HOSTNAME"; then
  # Create a new node for us,
  # Let the annealer do its thing.
  rebar nodes import "{\"name\": \"$HOSTNAME\", \"admin\": true, \"ip\": \"$IP\", \"bootenv\": \"local\"}"|| {
    echo "We could not create a node for ourself!"
    #exit 1
  }
else
  echo "Node already created, moving on"
fi

# does the rebar-joined-role exist?
if ! rebar nodes roles $HOSTNAME  |grep -q 'rebar-joined-node'; then
    rebar nodes bind "$HOSTNAME" to 'rebar-joined-node'
    if [ "$THE_LOCAL_NETWORK" != "" ] ; then
        rebar nodes bind "$HOSTNAME" to "network-${THE_LOCAL_NETWORK}"
    fi
fi
