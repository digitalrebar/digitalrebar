#!/bin/bash

if [[ ! -f /var/run/rebar/server.key ]]; then
    openssl genpkey -algorithm RSA -pkeyopt rsa_keygen_bits:2048 -out /var/run/rebar/server.key
    openssl req -new -key /var/run/rebar/server.key -out /var/run/rebar/server.csr -subj "/C=US/ST=Texas/L=Austin/O=RackN/OU=RebarAPI/CN=neode.net"
    openssl x509 -req -days 365 -in /var/run/rebar/server.csr -signkey /var/run/rebar/server.key -out /var/run/rebar/server.crt
    rm /var/run/rebar/server.csr
fi
chmod 400 /var/run/rebar/server.key /var/run/rebar/server.crt
chown rebar:rebar /var/run/rebar/server.key /var/run/rebar/server.crt
