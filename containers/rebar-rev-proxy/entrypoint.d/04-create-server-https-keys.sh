#!/bin/bash

mkdir -p /etc/rev-proxy
if [[ ! -f /etc/rev-proxy/server.key ]]; then
    openssl genpkey -algorithm RSA -pkeyopt rsa_keygen_bits:2048 -out /etc/rev-proxy/server.key
    openssl req -new -key /etc/rev-proxy/server.key -out /etc/rev-proxy/server.csr -subj "/C=US/ST=Texas/L=Austin/O=RackN/OU=RebarRevProxy/CN=neode.net"
    openssl x509 -req -days 365 -in /etc/rev-proxy/server.csr -signkey /etc/rev-proxy/server.key -out /etc/rev-proxy/server.crt
    rm /etc/rev-proxy/server.csr
fi
chmod 400 /etc/rev-proxy/server.key /etc/rev-proxy/server.crt
