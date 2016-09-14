#!/bin/bash

# Build initial access keys
if ! rebar users show machine-install; then
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

    if ! rebar users import "$machine_user"; then
        echo "Could not create machine-install user!"
        exit 1
    fi

    for cap in NODE_READ ROLE_READ NODE_CREATE NODE_UPDATE NODE_COMMIT \
                         DEPLOYMENT_READ DEPLOYMENT_UPDATE PROVIDER_READ; do
        blob="{\"user_id\":\"machine-install\",\"tenant_id\":\"system\",\"capability_id\":\"$cap\"}"
        rebar user_tenant_capabilitys create "$blob"
    done

    echo "machine-install:${MACHINE_PASSWORD}" >/etc/rebar.install.key
    kv_put digitalrebar/private/api/keys/machine_key </etc/rebar.install.key
else
    kv_get digitalrebar/private/api/keys/machine_key >/etc/rebar.install.key
fi
