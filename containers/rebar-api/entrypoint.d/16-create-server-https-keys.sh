#!/bin/bash


# Wait for key with certificate
sign-it -A -i -l internal -o /var/run/rebar/ca

sign-it -A -s -l internal -c rebar-api -h "rebarapi,rebar-api,rebar-api-service,$IP,${EXTERNAL_IP%%/*},${HOSTNAME},localhost,127.0.0.1" -o rebar-api

mv rebar-api.key /var/run/rebar/server.key
mv rebar-api.pem /var/run/rebar/server.crt
chmod 400 /var/run/rebar/server.key /var/run/rebar/server.crt /var/run/rebar/ca.pem
chown rebar:rebar /var/run/rebar/server.key /var/run/rebar/server.crt /var/run/rebar/ca.pem
