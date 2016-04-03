#!/bin/bash


# Wait for key with certificate
ca_string=$(kv_get "trust_me/certificate")
while [[ $ca_string == "" ]] ; do
    sleep 1
    ca_string=$(kv_get "trust_me/certificate")
done
echo "$ca_string" > /var/run/rebar/ca.pem

generate_crt "rebarapi" "rebar-api" "$IP,${EXTERNAL_IP%%/*},${HOSTNAME},localhost,127.0.0.1"

mv rebarapi-key.pem /var/run/rebar/server.key
mv rebarapi.pem /var/run/rebar/server.crt
mv rebarapi.csr /var/run/rebar/server.csr
chmod 400 /var/run/rebar/server.key /var/run/rebar/server.crt
chown rebar:rebar /var/run/rebar/server.key /var/run/rebar/server.crt /var/run/rebar/server.csr
