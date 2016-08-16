#!/bin/bash

# Force TFTP state tracking.  This may not be handled properly when
# doing packet forwarding in a container otherwise because reasons.
iptables -A PREROUTING -t raw -p udp --dport 69 -d "${FORWARDER_IP}" -j CT --helper tftp

cat >> /etc/consul.d/forwarder.json <<EOF
{
  "service": {
    "name": "forwarder",
    "tags": [ "deployment:$SERVICE_DEPLOYMENT" ],
    "address": "${FORWARDER_IP%%/*}",
    "check": {
      "script": "pgrep forwarder",
      "interval": "10s"
    }
  }
}
EOF
consul reload

/usr/local/bin/forwarder -ip ${FORWARDER_IP} &

