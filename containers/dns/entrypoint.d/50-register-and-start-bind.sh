#!/bin/bash

# If we are not using forwarder, we need to use the external address to take to us.
the_ip=${EXTERNAL_IP%%/*}
make_service "dns" "53" '{"script": "dig @127.0.0.1 127.0.0.1 >/dev/null 2>&1", "interval": "10s"}'
make_service "dns-mgmt" "6754" '{ "script": "pidof rebar-dns-mgmt","interval": "10s"}'

# Append DNS mgmt config from env vars
if [[ $DNS_TYPE == POWERDNS ]] ; then
    OTHER_PARMS="hostname = $DNS_SERVER_HOSTNAME\nport = $DNS_SERVER_PORT\npassword = $DNS_SERVER_TOKEN\n"
fi

cat >> /etc/dns-mgmt.conf <<EOF
type = $DNS_TYPE
server = $DNS_SERVER
$OTHER_PARMS
EOF

# Start the services.
/usr/local/bin/rebar-dns-mgmt --backing_store=consul --data_dir=digitalrebar/dns/database &
if [[ $DNS_TYPE == BIND ]] ; then
    /usr/sbin/named -g -u bind &
fi

bind_service dns-service
bind_service dns-mgmt_service

attr="{\"value\": [{
       \"address\": \"$the_ip\",
       \"port\": \"6754\",
       \"name\": \"$SERVICE_DEPLOYMENT\",
       \"access_name\": \"admin\",
       \"access_password\": \"admin\",
       \"url\": \"https://admin:admin@${the_ip}:6754\",
       \"cert\": $(jq -R -s '@text' </etc/dns-mgmt-https-cert.pem)
      }]
}"
# Make sure we set the token type
set_service_attrib dns-service dns-domain "{\"value\": \"$BASE_DOMAINNAME\"}"
set_service_attrib dns-mgmt_service dns-management-servers "$attr"
set_service_attrib dns-service dns_servers \
"{\"value\":[
    {\"address\": \"$the_ip\",
     \"port\": \"53\",
     \"name\": \"$SERVICE_DEPLOYMENT\"}]}"
