#!/bin/bash

if [[ $FORWARDER_IP ]]; then
    cp /root/internal-provisioner-service.json /etc/consul.d/provisioner.json
    cp /root/internal-tftp-service.json /etc/consul.d/tftp.json
else
    sed -e "s/FILLMEIN/${EXTERNAL_IP%%/*}/" root/external-provisioner-service.json >/etc/consul.d/provisioner.json
    sed -e "s/FILLMEIN/${EXTERNAL_IP%%/*}/" root/external-tftp-service.json >/etc/consul.d/tftp.json
fi
consul reload
rebar deployments commit system
