#!/bin/bash

prov_url="http://${EXTERNAL_IP%%/*}:${WEBPORT}"
mgmt_url="http://${EXTERNAL_IP%%/*}:${APIPORT}"

rebar deployments set system attrib provisioner-webservers to \
 "{\"value\": [{\"url\": \"$prov_url\", \"address\": \"${EXTERNAL_IP%%/*}\", \"port\": $WEBPORT}]}"

rebar deployments set system attrib provisioner-management-servers to \
 "{\"value\": [{\"url\": \"$mgmt_url\", \"address\": \"${EXTERNAL_IP%%/*}\", \"port\": $APIPORT}]}"
