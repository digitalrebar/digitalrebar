#!/bin/bash

cat >> /etc/consul.d/forwarder.json <<EOF
{
  "service": {
    "name": "forwarder",
    "tags": [ "deployment:system" ],
    "address": "${FORWARDER_IP%%/*}",
    "check": {
      "script": "pgrep forwarder",
      "interval": "10s"
    }
  }
}
EOF
consul reload

/usr/local/bin/forwarder -ip $FORWARDER_IP &

