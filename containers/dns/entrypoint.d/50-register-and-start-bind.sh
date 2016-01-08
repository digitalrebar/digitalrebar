#!/bin/bash

# If we are not using forwarder, we need to use the external address to take to us.
if [ "$FORWARDER_IP" == "" ] ; then
    the_ip=${EXTERNAL_IP%%/*}
    sed -e "s/FILLMEIN/$the_ip/" /root/dns-external.json > /etc/consul.d/dns.json
else 
    cp /root/dns-internal.json /etc/consul.d/dns.json
fi
consul reload

# Start the services.
/usr/local/bin/rebar-dns-mgmt &

ln -s /etc/init.d/bind9 /etc/init.d/named
service named start

# Make sure we set the token type
curl -X PUT -d 'BIND' http://127.0.0.1:8500/v1/kv/digitalrebar/private/dns/system/type?token=$CONSUL_M_ACL

# Access vars in consul for dns-mgmt
curl -X PUT -d 'admin' http://127.0.0.1:8500/v1/kv/digitalrebar/private/dns-mgmt/system/access_name?token=$CONSUL_M_ACL
curl -X PUT -d 'admin' http://127.0.0.1:8500/v1/kv/digitalrebar/private/dns-mgmt/system/access_password?$CONSUL_M_ACL
curl -X PUT --data-binary @/etc/dns-mgmt-https-cert.pem http://127.0.0.1:8500/v1/kv/digitalrebar/private/dns-mgmt/system/cert_pem?token=$CONSUL_M_ACL
