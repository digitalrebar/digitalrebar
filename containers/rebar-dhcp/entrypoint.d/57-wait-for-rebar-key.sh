#!/bin/bash

count=0
while [[ ! -e /etc/rebar-data/rebar-key.sh ]] ; do
  if [ $((count % 12)) -eq 0 ] ; then
      echo "Waiting for rebar-key.sh to show up"
  fi
  sleep 5
  count=$((count+1))
done

# Wait for the webserver to be ready.
. /etc/rebar-data/rebar-key.sh
count=0
while ! rebar ping &>/dev/null; do
  if [ $((count % 60)) -eq 0 ] ; then
      echo "Waiting for functional rebar-key.sh to show up"
  fi
  sleep 1
  . /etc/rebar-data/rebar-key.sh
  count=$((count+1))
done

# Add the DHCP services since we have one
rebar nodes bind "system-phantom.internal.local" to "dhcp-service"
rebar nodes bind "system-phantom.internal.local" to "dhcp-mgmt_service"
rebar nodes commit "system-phantom.internal.local"

