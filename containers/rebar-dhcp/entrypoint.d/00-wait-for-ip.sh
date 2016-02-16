#!/bin/bash

set +e
# Wait for external IP to show up in this container
ip addr show | grep inet | grep -v inet6 | awk '{ print $2 }' | grep -q "${EXTERNAL_IP}"
RC=$?
count=0
while [ $RC -ne 0 ] ; do
    sleep 1
    if [ $((count % 60)) -eq 0 ] ; then
        echo "Waiting for $EXTERNAL_IP"
    fi
    ip addr show | grep inet | grep -v inet6 | awk '{ print $2 }' | grep -q "${EXTERNAL_IP}"
    RC=$?
    count=$((count+1))
done
set -e

