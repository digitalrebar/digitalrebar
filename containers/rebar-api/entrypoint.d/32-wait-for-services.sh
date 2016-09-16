#!/bin/bash

all_services_present=false
role_ids=()
depl_id="$(rebar deployments show $SERVICE_DEPLOYMENT |jq '.id')"
for svc in $SERVICES; do
    role_ids+=($(rebar roles show "$svc" |jq '.id') )
done

while [[ $all_services_present != true ]]; do
    all_services_present=true
    for svc_id in "${role_ids[@]}"; do
        dr_id=$(rebar deploymentroles match "{\"role_id\": $svc_id, \"deployment_id\": $depl_id}" |jq '.[0].id')
        if [[ ! $dr_id || $dr_id = null ]]; then
            all_services_present=false
            sleep 1
            break
        fi
    done
done
