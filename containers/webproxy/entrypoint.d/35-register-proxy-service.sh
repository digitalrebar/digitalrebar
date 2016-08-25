#!/bin/bash

bind_service 'proxy-service'
attr="{\"value\": [{
    \"address\": \"${EXTERNAL_IP%%/*}\",
    \"port\": \"3128\",
    \"url\": \"http://${EXTERNAL_IP%%/*}:3128\"
    }]}"

set_service_attrib proxy-service proxy-servers "$attr"

