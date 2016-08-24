#!/bin/bash

while ! rebar -U rebar -P rebar1 ping; do
    sleep 5
done

 
# Add rev-proxy matcher
OSD=$SERVICE_DEPLOYMENT
SERVICE_DEPLOYMENT="$OSD\", \"revproxy-default"
make_revproxied_service "rebar-api" 3000 '{"script": "rebar -T -E $REBAR_ENDPOINT -U system ping","interval": "10s"}'
SERVICE_DEPLOYMENT=$OSD



