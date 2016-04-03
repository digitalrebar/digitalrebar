#!/bin/bash

make_service "trust-me" "8888" '{"script": "curl -X PUT -d \"{}\" http://localhost:8888/api/v1/cfssl/info","interval": "10s"}'

kv_put trust_me/certificate < /var/cache/trust_me/ca.pem

