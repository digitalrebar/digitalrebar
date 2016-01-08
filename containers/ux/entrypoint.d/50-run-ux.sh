#!/bin/bash

if [ "$forwarder" != "" ] ; then
    ip route del default
    ip route add default via $forwarder
fi

cat >> /etc/consul.d/rebar-ux.json <<EOF
{
  "service": {
    "name": "internal-rebar-ux",
    "tags": [ "deployment:system" ],
    "port": 443,
    "check": {
      "script": "curl -k -H 'Host=www.mydomain.com' https://localhost:443",
      "interval": "10s"
    }
  }
}
EOF

run_forever() (
    while true; do
        "$@"
        sleep 5
    done
)

consul reload
cd /opt/digitalrebar-ux

bower --allow-root install --config.interactive=false

EIP=${EXTERNAL_IP%/*}
FIP=${FORWARDER_IP%/*}

cat > example.conf <<EOF
[ req ]
default_bits        = 2048
default_keyfile     = server-key.pem
distinguished_name  = subject
req_extensions      = extensions
x509_extensions     = extensions
string_mask         = utf8only

[ subject ]
countryName         = US
countryName_default     = US

stateOrProvinceName     = TX
stateOrProvinceName_default = TX

localityName            = Austin
localityName_default        = Austin

rganizationName         = RackN
organizationName_default    = RackN

commonName          = RackN UX
commonName_default      = RackN UX

emailAddress            = Email Address
emailAddress_default        = support@rackn.com

[ extensions ]

subjectKeyIdentifier        = hash
authorityKeyIdentifier  = keyid,issuer

basicConstraints        = CA:FALSE
keyUsage            = nonRepudiation, digitalSignature, keyEncipherment
extendedKeyUsage    = serverAuth
subjectAltName          = @alternate_names
nsComment           = "OpenSSL Generated Certificate"

[ alternate_names ]

DNS.1       = localhost
DNS.2       = 127.0.0.1
DNS.3       = ${EIP:-127.0.1.1}
DNS.4       = ${FIP:-127.0.2.1}
EOF

openssl req -config example.conf -new -x509 -newkey rsa:2048 -keyout server.pem -out server.pem -days 365 -nodes -subj "/C=US/ST=Texas/L=Austin/O=RackN/OU=UX/CN=rackn.com"

touch websecureport.log
run_forever python simple-https.py >websecureport.log 2>&1 &

tail -f websecureport.log

