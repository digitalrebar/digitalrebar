#!/bin/bash

ACCESS_ARGS="--cacert /etc/prov-base-cert.pem --cert /etc/prov-cert.pem --key /etc/prov-key.pem"

# Load templates
for f in "/opt/provisioner-mgmt/templates"/*.tmpl; do
    curl $ACCESS_ARGS -X POST --data-binary "@$f" \
         "https://localhost:$APIPORT/templates/${f##*/}?tenant_id=1" || :
done

# Load bootenvs
for f in "/opt/provisioner-mgmt/bootenvs"/*.json; do
    curl $ACCESS_ARGS -X POST --data-binary "@$f" \
         -H 'Content-Type: application/json' \
         https://localhost:$APIPORT/bootenvs || :
done
