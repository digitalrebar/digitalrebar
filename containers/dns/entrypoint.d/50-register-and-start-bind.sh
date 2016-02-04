#!/bin/bash

# If we are not using forwarder, we need to use the external address to take to us.
the_ip=${EXTERNAL_IP%%/*}
if [[ $FORWARDER_IP ]] ; then
    cp /root/dns-internal.json /etc/consul.d/dns.json
    cp /root/dns-mgmt-internal.json /etc/consul.d/dns-mgmt.json
else
    sed -e "s/FILLMEIN/$the_ip/" /root/dns-external.json > /etc/consul.d/dns.json
    sed -e "s/FILLMEIN/$the_ip/" /root/dns-mgmt-external.json > /etc/consul.d/dns-mgmt.json
fi

consul reload

# Append DNS mgmt config from env vars
if [[ $DNS_TYPE == POWERDNS ]] ; then
    OTHER_PARMS="hostname = $DNS_SERVER_HOSTNAME\nport = $DNS_SERVER_PORT\npassword = $DNS_SERVER_TOKEN\n"
fi

cat >> /etc/dns-mgmt.conf <<EOF
type = $DNS_TYPE
server = $DNS_SERVER
$OTHER_PARMS
EOF

# Start the services.
/usr/local/bin/rebar-dns-mgmt --backing_store=consul --data_dir=digitalrebar/dns/database &
if [[ $DNS_TYPE == BIND ]] ; then
    /usr/sbin/named -g -u bind &
fi

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
rebar deployments set system attrib dns_servers to \
"{\"value\":[
    {\"address\": \"$the_ip\",
     \"port\": \"53\",
     \"name\": \"system\"}]}"

rebar deployments commit system
