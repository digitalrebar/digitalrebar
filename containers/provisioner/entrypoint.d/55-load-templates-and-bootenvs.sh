#!/bin/bash

# Load templates
for f in "$GOPATH/src/github.com/rackn/provisioner-mgmt/templates"/*.tmpl; do
    curl -X POST --data-binary "@$f" \
         "http://localhost:$APIPORT/templates/${f##*/}" || :
done

# Load bootenvs
for f in "$GOPATH/src/github.com/rackn/provisioner-mgmt/bootenvs"/*.json; do
    curl -X POST --data-binary "@$f" \
         -H 'Content-Type: application/json' \
         http://localhost:$APIPORT/bootenvs || :
done
