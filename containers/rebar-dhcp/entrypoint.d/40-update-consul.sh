#!/bin/bash

attr="{\"value\": [{
       \"address\": \"${EXTERNAL_IP%%/*}\",
       \"port\": \"6755\",
       \"name\": \"$SERVICE_DEPLOYMENT\",
       \"url\": \"https://${EXTERNAL_IP%%/*}:6755\"
      }]
}"
# Make sure we set the token type
set_service_attrib dhcp-service dhcp_servers "{\"value\": [\"${EXTERNAL_IP%%/*}\"]}"
set_service_attrib dhcp-mgmt_service dhcp-management-servers "$attr"

