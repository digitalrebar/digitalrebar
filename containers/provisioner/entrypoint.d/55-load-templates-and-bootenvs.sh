#!/bin/bash

# Load templates iff they are not already loaded.
for f in "/opt/provisioner-mgmt/templates"/*.tmpl; do
    rebar provisioner templates exists "${f##*/}" || \
       rebar provisioner templates upload "$f" as "${f##*/}" || :
done

# Load bootenvs iff they are not already loaded.
for f in "/opt/provisioner-mgmt/bootenvs"/*.json; do
    bootenv_name="$(jq -r -c '.Name' <"$f")"
    rebar provisioner bootenvs exists "$bootenv_name" || \
        rebar provisioner bootenvs create - < "$f" || :

done
unset bootenv_name
unset f
rm -rf /tmp/prov-temp*
