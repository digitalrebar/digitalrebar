#!/bin/bash

# Wait for key with certificate
ca_string=$(kv_get "trust_me/certificate")
while [[ $ca_string == "" ]] ; do
    sleep 1
    ca_string=$(kv_get "trust_me/certificate")
done
echo "$ca_string" > /etc/prov-base-cert.pem

generate_crt "prov" "provisioner" "$IP,${EXTERNAL_IP%%/*},${HOSTNAME},127.0.0.1,localhost"

mv prov.pem /etc/prov-cert.pem
mv prov.csr /etc/prov-csr.json
mv prov-key.pem /etc/prov-key.pem

chmod 600 /etc/prov-*
