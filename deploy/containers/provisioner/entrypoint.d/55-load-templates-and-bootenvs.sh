#!/bin/bash

# Wait for key with certificate
sign-it -A -i -l internal -o /tmp/prov-temp-ca

sign-it -A -s -l internal -c prov-temp -h "rebarapi,rebar-api,rebar-api-service,$IP,${EXTERNAL_IP%%/*},${HOSTNAME},localhost,127.0.0.1" -o /tmp/prov-temp

ACCESS_ARGS="--cacert /tmp/prov-temp-ca.pem --cert /tmp/prov-temp.pem --key /tmp/prov-temp.key"

caps="{\"1\": { \"capabilities\": [\"BOOTENV_CREATE\",\"BOOTENV_UPDATE\",\"TEMPLATE_CREATE\",\"TEMPLATE_UPDATE\"], \"parent\": null}}"

# Load templates
for f in "/opt/provisioner-mgmt/templates"/*.tmpl; do
    curl $ACCESS_ARGS -X POST --data-binary "@$f" \
        -H "X-Authenticated-Username: system" \
        -H "X-Authenticated-Capability: $caps" \
        "https://localhost:$APIPORT/templates/${f##*/}?tenant_id=1" || :
done

# Load bootenvs
for f in "/opt/provisioner-mgmt/bootenvs"/*.json; do
    curl $ACCESS_ARGS -X POST --data-binary "@$f" \
        -H 'Content-Type: application/json' \
        -H "X-Authenticated-Username: system" \
        -H "X-Authenticated-Capability: $caps" \
        https://localhost:$APIPORT/bootenvs || :
done

rm -rf /tmp/prov-temp*
