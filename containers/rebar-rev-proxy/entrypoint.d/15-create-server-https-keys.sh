#!/bin/bash

mkdir -p /etc/rev-proxy

# Wait for key with certificate
ca_string=$(kv_get "trust_me/certificate")
while [[ $ca_string == "" ]] ; do
    sleep 1
    ca_string=$(kv_get "trust_me/certificate")
done
echo "$ca_string" > /etc/rev-proxy/ca.pem

generate_crt "rrp" "rev-proxy" "$IP,${EXTERNAL_IP%%/*},${HOSTNAME},127.0.0.1,localhost"

mv rrp.pem /etc/rev-proxy/server.crt
mv rrp.csr /etc/rev-proxy/server.csr
mv rrp-key.pem /etc/rev-proxy/server.key

chmod 600 /etc/rev-proxy/*
