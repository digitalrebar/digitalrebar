#!/bin/bash

# Build initial access keys
if ! rebar -U rebar -P rebar1 users show machine-install; then
    echo "Creating machine-install user"
    MACHINE_PASSWORD=$(dd if=/dev/urandom bs=64 count=1 2>/dev/null | sha512sum - 2>/dev/null | awk '{ print $1 }')
    machine_user="
{
  \"username\": \"machine-install\",
  \"email\": \"root@localhost.localdomain\",
  \"password\": \"${MACHINE_PASSWORD}\",
  \"password_confirmation\": \"${MACHINE_PASSWORD}\",
  \"remember_me\": false,
  \"is_admin\": true,
  \"digest\": true
}"

    if ! rebar -U rebar -P rebar1 -E https://127.0.0.1:3000 users import "$machine_user"; then
        echo "Could not create machine-install user!"
        exit 1
    fi

    rebar -U rebar -P rebar1 -E https://127.0.0.1:3000 user_tenant_capabilitys create '{ "user_id": 2, "tenant_id": 1, "capability_id": "NODE_READ" }'
    rebar -U rebar -P rebar1 -E https://127.0.0.1:3000 user_tenant_capabilitys create '{ "user_id": 2, "tenant_id": 1, "capability_id": "NODE_CREATE" }'
    rebar -U rebar -P rebar1 -E https://127.0.0.1:3000 user_tenant_capabilitys create '{ "user_id": 2, "tenant_id": 1, "capability_id": "NODE_UPDATE" }'
    rebar -U rebar -P rebar1 -E https://127.0.0.1:3000 user_tenant_capabilitys create '{ "user_id": 2, "tenant_id": 1, "capability_id": "NODE_COMMIT" }'

    echo "machine-install:${MACHINE_PASSWORD}" >/etc/rebar.install.key
    kv_put digitalrebar/private/api/keys/machine_key </etc/rebar.install.key
else
    kv_get digitalrebar/private/api/keys/machine_key >/etc/rebar.install.key
fi
