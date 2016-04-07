#!/bin/bash

while ! rebar -U rebar -P rebar1 ping; do
    sleep 5
done

 
# Add rev-proxy matcher
echo '^rebarapi/(.*)' | kv_put digitalrebar/public/revproxy/rebar-api-service/matcher

OSD=$SERVICE_DEPLOYMENT
SERVICE_DEPLOYMENT="$OSD\", \"revproxy\", \"revproxy-default"
make_service rebar-api 3000 '{"script": "rebar -E $REBAR_ENDPOINT -U rebar -P rebar1 ping","interval": "10s"}'
SERVICE_DEPLOYMENT=$OSD

