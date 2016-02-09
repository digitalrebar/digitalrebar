#!/bin/bash

make_service dhcp 67 '{"script": "pidof rebar-dhcp", "interval": "10s"}'
make_service dhcp-mgmt  6755 '{"script": "pidof rebar-dhcp", "interval": "10s"}'
consul reload

attr="{\"value\": [{
       \"address\": \"${EXTERNAL_IP%%/*}\",
       \"port\": \"6755\",
       \"name\": \"$SERVICE_DEPLOYMENT\",
       \"access_name\": \"admin\",
       \"access_password\": \"admin\",
       \"url\": \"https://admin:admin@${EXTERNAL_IP%%/*}:6755\",
       \"cert\": $(jq -R -s '@text' </etc/dhcp-https-cert.pem)
      }]
}"
# Make sure we set the token type
set_service_attrib dhcp-service dhcp_servers "{\"value\": [\"${EXTERNAL_IP%%/*}\"]}"
set_service_attrib dhcp-mgmt_service dhcp-management-servers "$attr"

