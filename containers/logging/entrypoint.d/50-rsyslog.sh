#!/bin/bash

if [[ $FORWARDER_IP ]]; then
    cp /root/internal-logging.json /etc/consul.d/logging.json
    cp /root/internal-logging-relp.json /etc/consul.d/logging-relp.json
else
    sed -e "s/FILLMEIN/${EXTERNAL_IP%%/*}/" root/external-logging.json >/etc/consul.d/logging.json
    sed -e "s/FILLMEIN/${EXTERNAL_IP%%/*}/" root/external-logging-relp.json >/etc/consul.d/logging-relp.json
fi
consul reload

/usr/sbin/rsyslogd

rebar nodes bind system-phantom.internal.local to logging-service
rebar deployments set system attrib logging_servers to "{\"value\": [\"${EXTERNAL_IP%%/*}\"]}"
rebar deployments commit system
