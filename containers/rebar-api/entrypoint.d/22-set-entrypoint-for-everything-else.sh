#!/bin/bash
(
    if ! kv_get digitalrebar/private/api/keys/rebar_key >/etc/rebar-data/rebar-key.sh; then
        export REBAR_KEY="machine-install:$(dd if=/dev/urandom bs=64 count=1 2>/dev/null | sha512sum - 2>/dev/null | awk '{ print $1 }')"
        export REBAR_KEY="rebar:rebar1" # GREG: THIS IS TEMPORARY
        touch /tmp/.do_config
    else
        rm /tmp/.do_config || :
        . /etc/rebar-data/rebar-key.sh
    fi

    cat >/etc/rebar-data/rebar-key.sh <<EOF
export REBAR_KEY="$REBAR_KEY"
export REBAR_ENDPOINT="https://${IP}:3000"
export EXTERNAL_REBAR_ENDPOINT="https://${EXTERNAL_IP%%/*}:3000"
EOF
    kv_put digitalrebar/private/api/keys/rebar_key </etc/rebar-data/rebar-key.sh
)
. /etc/rebar-data/rebar-key.sh
