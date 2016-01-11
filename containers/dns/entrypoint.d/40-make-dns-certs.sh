#!/bin/bash

echo "IP.0=$IP" >> /etc/dns-mgmt-cert.conf
echo "IP.1=${EXTERNAL_IP%%/*}" >> /etc/dns-mgmt-cert.conf
echo "DNS.0=$IP" >> /etc/dns-mgmt-cert.conf
echo "DNS.1=$HOSTNAME" >> /etc/dns-mgmt-cert.conf
echo "DNS.2=${EXTERNAL_IP%%/*}" >> /etc/dns-mgmt-cert.conf

openssl req -nodes -sha256 -x509 -newkey rsa:2048 \
   -keyout /etc/dns-mgmt-https-key.pem -out /etc/dns-mgmt-https-cert.pem -days 1001 \
   -subj "/C=US/ST=Denial/L=Anytown/O=Dis/CN=admin" -config /etc/dns-mgmt-cert.conf
chmod 600 /etc/dns-mgmt*
