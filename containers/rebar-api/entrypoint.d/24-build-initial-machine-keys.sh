#!/bin/bash

# Build initial access keys
if ! rebar -U rebar -P rebar1 users show machine-install; then
    echo "Creating machine-install user"
    machine_user="
{
  \"username\": \"${REBAR_KEY%%:*}\",
  \"email\": \"root@localhost.localdomain\",
  \"password\": \"${REBAR_KEY#*:}\",
  \"password_confirmation\": \"${REBAR_KEY#*:}\",
  \"remember_me\": false,
  \"is_admin\": true,
  \"digest\": true
}"

    if ! rebar -U rebar -P rebar1 -E https://127.0.0.1:3000 users import "$machine_user"; then
        echo "Could not create machine-install user!"
        exit 1
    fi

    echo "$REBAR_KEY" >/etc/rebar.install.key
    kv_put digitalrebar/private/api/keys/machine_key </etc/rebar.install.key
else
    kv_get digitalrebar/private/api/keys/machine_key >/etc/rebar.install.key
fi
