#!/bin/bash

cd /var/cache/trust_me
sed "s/abcdef0123456789abcdef0123456789/$CERT_AUTH_KEY/g" /etc/trust_me/config_ca.json > config_ca.json
cfssl serve -address 0.0.0.0 -port 8888 -ca-key ca-key.pem -ca ca.pem -config config_ca.json &
cd -

