#!/bin/bash

if ! kv_get digitalrebar/private/api/keys/rebar_key &>/dev/null; then
# Copy out stuff to data dir
    cat >/etc/rebar-data/rebar-key.sh <<EOF
export REBAR_KEY="$REBAR_KEY"
export REBAR_ENDPOINT="$REBAR_ENDPOINT"
export EXTERNAL_REBAR_ENDPOINT="https://${EXTERNAL_IP%%/*}:3000"
EOF
    kv_put digitalrebar/private/api/keys/rebar_key </etc/rebar-data/rebar-key.sh
fi
