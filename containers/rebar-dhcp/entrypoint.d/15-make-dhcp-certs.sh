#!/bin/bash

# Wait for key with certificate
ca_string=$(kv_get "trust_me/certificate")
while [[ $ca_string == "" ]] ; do
    sleep 1
    ca_string=$(kv_get "trust_me/certificate")
done
echo "$ca_string" > /etc/dhcp-base-cert.pem


HOSTS=`hostname`
HOSTS="$HOSTS,localhost"
I_IP=`ip addr show | grep inet | grep -v inet6 | awk '{ print $2 }' | awk -F/ '{ print $1 }'|sed "s/ /,/g"`
CI_IP=`echo $I_IP | sed "s/ /,/g"`
HOSTS="$HOSTS,$CI_IP"

generate_crt "dhcp" "dhcp-mgmt" "$HOSTS"

mv dhcp.pem /etc/dhcp-https-cert.pem
mv dhcp.csr /etc/dhcp-https-csr.json
mv dhcp-key.pem /etc/dhcp-https-key.pem

chmod 600 /etc/dhcp-*
