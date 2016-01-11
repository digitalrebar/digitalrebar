#!/bin/bash

if [ "$FORWARDER_IP" == "" ] ; then
    sed -e "s/FILLMEIN/${EXTERNAL_IP%%/*}/" /root/squid-external.json > /etc/consul.d/squid.json
else
    cp /root/squid-internal.json /etc/consul.d/squid.json
fi
consul reload

