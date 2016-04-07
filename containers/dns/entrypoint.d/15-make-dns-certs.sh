#!/bin/bash

# Wait for key with certificate
ca_string=$(kv_get "trust_me/certificate")
while [[ $ca_string == "" ]] ; do
    sleep 1
    ca_string=$(kv_get "trust_me/certificate")
done
echo "$ca_string" > /etc/dns-mgmt-base-cert.pem

generate_crt "dns" "dns-mgmt" "dns,dns-mgmt,dns-mgmt-service,$IP,${EXTERNAL_IP%%/*},${HOSTNAME},127.0.0.1,localhost"

mv dns.pem /etc/dns-mgmt-https-cert.pem
mv dns.csr /etc/dns-mgmt-https-csr.json
mv dns-key.pem /etc/dns-mgmt-https-key.pem

chmod 600 /etc/dns-mgmt*
