#!/bin/bash

service_dhcp="{
  \"Node\": \"dhcp\",
  \"Address\": \"${EXTERNAL_IP%%/*}\",
  \"Service\": {
    \"ID\": \"dhcp\",
    \"Service\": \"dhcp\",
    \"Tags\": [ \"deployment:system\" ],
    \"Address\": \"${EXTERNAL_IP%%/*}\",
    \"Port\": 67
  }
}"

service_dhcp_mgmt="{
  \"Node\": \"dhcp\",
  \"Address\": \"${EXTERNAL_IP%%/*}\",
  \"Service\": {
    \"ID\": \"dhcp-mgmt-service\",
    \"Service\": \"dhcp-mgmt-service\",
    \"Tags\": [ \"deployment:system\" ],
    \"Address\": \"${EXTERNAL_IP%%/*}\",
    \"Port\": 6755
  }
}"

attr="{\"value\": [{
       \"address\": \"${EXTERNAL_IP%%/*}\",
       \"port\": \"6755\",
       \"name\": \"system\",
       \"access_name\": \"admin\",
       \"access_password\": \"admin\",
       \"url\": \"https://admin:admin@${EXTERNAL_IP%%/*}:6755\",
       \"cert\": $(jq -R -s '@text' </etc/dhcp-https-cert.pem)
      }]
}"
# Make sure we set the token type
rebar deployments set system attrib dhcp-management-servers to "$attr"
rebar deployments commit system

# Add service
curl -X PUT -d "$service_dhcp" http://127.0.0.1:8500/v1/catalog/register?token=$CONSUL_M_ACL
curl -X PUT -d "$service_dhcp_mgmt" http://127.0.0.1:8500/v1/catalog/register?token=$CONSUL_M_ACL

# Access vars in consul for rebar-dhcp
curl -X PUT -d 'admin' http://127.0.0.1:8500/v1/kv/digitalrebar/private/dhcp-mgmt/system/access_name?token=$CONSUL_M_ACL
curl -X PUT -d 'admin' http://127.0.0.1:8500/v1/kv/digitalrebar/private/dhcp-mgmt/system/access_password?$CONSUL_M_ACL
curl -X PUT --data-binary @/etc/dhcp-https-cert.pem http://127.0.0.1:8500/v1/kv/digitalrebar/private/dhcp-mgmt/system/cert_pem?token=$CONSUL_M_ACL
