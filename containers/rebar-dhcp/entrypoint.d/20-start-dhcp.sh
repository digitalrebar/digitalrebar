#!/bin/bash

# Service has to start first - or we should move the consul stuff into the app.
/usr/local/bin/rebar-dhcp --server_ip=$EXTERNAL_IP --backing_store=consul --data_dir=digitalrebar/dhcp/database --auth_mode=KEY &

check_line="{\"tcp\": \"${IP%/*}:6755\", \"interval\": \"10s\", \"timeout\": \"2s\"}"
make_service dhcp 67 "$check_line"
make_service dhcp-mgmt  6755 "$check_line"

