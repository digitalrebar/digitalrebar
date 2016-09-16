#!/bin/bash

# The IP to expose
the_ip=${EXTERNAL_IP%%/*}

if [[ $EXTERNAL_DNS_SERVERS == '' ]] ; then
    echo "Must specify external DNS servers"
    exit -1
fi

IFS=', ' read -r -a external_server_array <<< "$EXTERNAL_DNS_SERVERS"

DNS_MGMT_PORT=${DNS_MGMT_PORT:-6754}

# If we aren't bind, make that the server to talk to.
if [[ $DNS_TYPE == BIND ]] ; then
    dns_ip=$the_ip
    DNS_SERVER_PARM=$DNS_BIND_SERVER_NAME

    # Update forwarders line
    data=""
    for element in "${external_server_array[@]}"
    do
        data="${data}${element};\\n"
    done
    sed -e "s/^EXTERNAL_DNS_SERVERS/${data}/g" /etc/bind/named.conf.tmpl > /etc/bind/named.conf
    sed -e "s/^EXTERNAL_DNS_SERVERS/${data}/g" /etc/bind/named.conf.tmpl > /etc/named.conf

    /usr/sbin/named -g -u bind &

    IFS=', ' read -r -a external_server_array <<< "$dns_ip"

elif [[ $DNS_TYPE == POWERDNS ]] ; then
    dns_ip=$DNS_SERVER_HOSTNAME
    DNS_SERVER_PARM=$DNS_SERVER_NAME
elif [[ $DNS_TYPE == NSUPDATE ]] ; then
    dns_ip=$DNS_NSUPDATE_IP
    DNS_SERVER_PARM=$DNS_NSUPDATE_IP
elif [[ $DNS_TYPE == EXTERNAL ]] ; then
    dns_ip=${external_server_array[0]}
fi

#
# If we aren't external, we need to start a management service.
#
if [[ $DNS_TYPE != EXTERNAL ]] ; then
    # Append DNS mgmt config from env vars
    if [[ $DNS_TYPE == POWERDNS ]] ; then
        OTHER_PARMS="--dnsHostname=$DNS_SERVER_HOSTNAME --dnsPort=$DNS_SERVER_PORT dnsPassword=$DNS_SERVER_TOKEN"
    fi
    BACKEND_ARGS="--dnsType=$DNS_TYPE --dnsServer=$DNS_SERVER_PARM $OTHER_PARAM"

    rebar-dns-mgmt $BACKEND_ARGS --backingStore=consul --dataDir=digitalrebar/dns/database --host="dns,dns-mgmt,dns-mgmt-service,$IP,${EXTERNAL_IP%%/*},${HOSTNAME},127.0.0.1,localhost" &

    make_revproxied_service "dns-mgmt" $DNS_MGMT_PORT '{ "script": "pidof rebar-dns-mgmt","interval": "10s"}'
    # Alas, we still need this for now.
    bind_service dns-mgmt_service
    mgmt_attr="{\"value\": [{
       \"address\": \"$the_ip\",
       \"port\": \"$DNS_MGMT_PORT\",
       \"name\": \"$SERVICE_DEPLOYMENT\",
       \"url\": \"https://${the_ip}:$DNS_MGMT_PORT\"
      }]
}"
    set_service_attrib dns-mgmt_service dns-management-servers "$mgmt_attr"
fi

attr="{\"value\":["
COMMA=""
for element in "${external_server_array[@]}"
do
    attr="${attr}${COMMA}
    {\"address\": \"$element\",
     \"port\": \"53\",
     \"name\": \"$SERVICE_DEPLOYMENT\"}"
    COMMA=","
done
attr="${attr}]}"

make_service "dns" "53" "{\"script\": \"dig @$dns_ip $dns_ip >/dev/null 2>&1\", \"interval\": \"10s\"}"
bind_service dns-service
set_service_attrib dns-service dns-domain "{\"value\": \"$BASE_DOMAINNAME\"}"
set_service_attrib dns-service dns_servers "${attr}"

