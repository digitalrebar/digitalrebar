#!/bin/bash
make_service "provisioner" $WEBPORT '{"script": "pidof sws", "interval": "10s"}'

make_revproxied_service "provisioner-mgmt" $APIPORT '{"script": "pidof provisioner-mgmt","interval": "10s"}'
    
make_service "provisioner-tftp" 69 '{"script": "pidof sws","interval": "10s"}'

