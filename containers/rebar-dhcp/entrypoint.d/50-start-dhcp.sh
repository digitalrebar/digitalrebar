#!/bin/bash

HOSTNAME=`hostname`
IP=`ip addr show | grep inet | grep -v inet6 | awk '{ print $2 }' | awk -F/ '{ print $1 }'`
E_IP=${EXTERNAL_IP%%/*}

count=0
echo "IP.$count=$E_IP" >> /etc/rebar-dhcp-cert.conf
count=$((count+1))
echo "$IP" | while read line ; do
    echo "IP.$count=$line" >> /etc/rebar-dhcp-cert.conf
    count=$((count+1))
done

count=0
echo "DNS.$count=$HOSTNAME" >> /etc/rebar-dhcp-cert.conf
count=$((count+1))
echo "DNS.$count=$E_IP" >> /etc/rebar-dhcp-cert.conf
count=$((count+1))
echo "$IP" | while read line ; do
    echo "DNS.$count=$line" >> /etc/rebar-dhcp-cert.conf
    count=$((count+1))
done

openssl req -nodes -sha256 -x509 -newkey rsa:2048 \
   -keyout /etc/dhcp-https-key.pem -out /etc/dhcp-https-cert.pem -days 1001 \
   -subj "/C=US/ST=Denial/L=Anytown/O=Dis/CN=admin" -config /etc/rebar-dhcp-cert.conf
chmod 600 /etc/rebar-dhcp*

# Service has to start first - or we should move the consul stuff into the app.
/usr/local/bin/rebar-dhcp --server_ip=$EXTERNAL_IP &

