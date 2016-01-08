#!/bin/bash

# Setup database tasks
. /opt/digitalrebar/core/setup/00-rebar-rake-tasks.install

# Start up the code
. /opt/digitalrebar/core/setup/01-rebar-start.install

# Build initial access keys
. /opt/digitalrebar/core/setup/02-make-machine-key.install

[[ $REBAR_ENDPOINT ]] || export REBAR_ENDPOINT="https://${IP}:3000"

if [[ $FORWARDER_IP ]]; then
    cat >/etc/consul.d/rebar-api.json <<EOF
{
  "service": {
    "name": "internal-rebar-api-service",
    "tags": [ "system" ],
    "port": 3000,
    "check": {
      "script": "rebar -E $REBAR_ENDPOINT -U rebar -P rebar1 ping",
      "interval": "10s"
    }
  }
}
EOF
else
    cat >/etc/consul.d/rebar-api.json <<EOF
{
  "service": {
    "name": "rebar-api-service",
    "address": "${EXTERNAL_IP%%*/}",
    "tags": [ "system" ],
    "port": 3000,
    "check": {
      "script": "rebar -E $REBAR_ENDPOINT -U rebar -P rebar1 ping",
      "interval": "10s"
    }
  }
}
EOF
fi
consul reload
/opt/digitalrebar/core/rebar-docker-install.sh
