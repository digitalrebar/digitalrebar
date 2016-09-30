#!/bin/bash

# Replace this with the Rebar CLI once it knows how to talk to the provisioner
# Wait for key with certificate
sign-it -A -i -l internal -o /tmp/prov-temp-ca

sign-it -A -s -l internal -c prov-temp -h "rebarapi,rebar-api,rebar-api-service,$IP,${EXTERNAL_IP%%/*},${HOSTNAME},localhost,127.0.0.1" -o /tmp/prov-temp

do_curl() {
    local caps="{\"1\": { \"capabilities\": [\"BOOTENV_CREATE\",\"BOOTENV_UPDATE\",\"TEMPLATE_CREATE\",\"TEMPLATE_UPDATE\"], \"parent\": null}}"
    curl --cacert /tmp/prov-temp-ca.pem \
         --cert /tmp/prov-temp.pem \
         --key /tmp/prov-temp.key \
         -H "X-Authenticated-Username: system" \
         -H "X-Authenticated-Capability: $caps" \
         "$@"
}

# Load templates iff they are not already loaded.
for f in "/opt/provisioner-mgmt/templates"/*.tmpl; do
    uri="https://localhost:$APIPORT/templates/${f##*/}?tenant_id=1"
    do_curl -fgL "$uri" || do_curl -X POST --data-binary "@$f" "$uri" || :
done

# Load bootenvs iff they are not already loaded.
for f in "/opt/provisioner-mgmt/bootenvs"/*.json; do
    bootenv_name="$(jq -r -c '.Name' <"$f")"
    uri="https://localhost:$APIPORT/bootenvs"
    do_curl -fgL "${uri}/${bootenv_name}" || \
        do_curl -X POST --data-binary "@$f" \
                -H 'Content-Type: application/json' \
                "$uri" || :

done
unset uri
unset bootenv_name
rm -rf /tmp/prov-temp*
