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
/usr/local/bin/rebar-dns-mgmt --backing_store=consul --data_dir=digitalrebar/dns/database &
/usr/sbin/named -g -u bind &

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
