#!/bin/bash

# If we are not using forwarder, we need to use the external address to take to us.
if [[ $FORWARDER_IP ]] ; then
    the_ip=${IP%%/*}
    cp /root/dns-internal.json /etc/consul.d/dns.json
    cp /root/dns-mgmt-internal.json /etc/consul.d/dns-mgmt.json
else
    the_ip=${EXTERNAL_IP%%/*}
    sed -e "s/FILLMEIN/$the_ip/" /root/dns-external.json > /etc/consul.d/dns.json
    sed -e "s/FILLMEIN/$the_ip/" /root/dns-mgmt-external.json > /etc/consul.d/dns-mgmt.json
fi
consul reload

# Start the services.
/usr/local/bin/rebar-dns-mgmt &

service bind9 start

attr="{\"value\": [{
       \"address\": \"$the_ip\",
       \"port\": \"6754\",
       \"name\": \"system\",
       \"access_name\": \"admin\",
       \"access_password\": \"admin\",
       \"url\": \"https://admin:admin@${the_ip}:6754\",
       \"cert\": $(jq -R -s '@text' </etc/dns-mgmt-https-cert.pem)
      }]
}"
# Make sure we set the token type
rebar deployments set system attrib dns-management-servers to "$attr"
rebar deployments commit system

curl -X PUT -d 'BIND' http://127.0.0.1:8500/v1/kv/digitalrebar/private/dns/system/type?token=$CONSUL_M_ACL

# Access vars in consul for dns-mgmt
curl -X PUT -d 'admin' http://127.0.0.1:8500/v1/kv/digitalrebar/private/dns-mgmt/system/access_name?token=$CONSUL_M_ACL
curl -X PUT -d 'admin' http://127.0.0.1:8500/v1/kv/digitalrebar/private/dns-mgmt/system/access_password?$CONSUL_M_ACL
curl -X PUT --data-binary @/etc/dns-mgmt-https-cert.pem http://127.0.0.1:8500/v1/kv/digitalrebar/private/dns-mgmt/system/cert_pem?token=$CONSUL_M_ACL
