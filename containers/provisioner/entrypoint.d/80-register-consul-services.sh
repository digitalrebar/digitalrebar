#!/bin/bash
make_service "provisioner" $WEBPORT '{"script": "pidof sws", "interval": "10s"}'


# Add rev-proxy matcher
echo '^provisioner/(.*)' | kv_put digitalrebar/public/revproxy/provisioner-mgmt-service/matcher

OSD=$SERVICE_DEPLOYMENT
SERVICE_DEPLOYMENT="$OSD\", \"revproxy"
make_service "provisioner-mgmt" $APIPORT '{"script": "pidof provisioner-mgmt","interval": "10s"}'
SERVICE_DEPLOYMENT=$OSD
    
make_service "provisioner-tftp" 69 '{"script": "pidof sws","interval": "10s"}'

