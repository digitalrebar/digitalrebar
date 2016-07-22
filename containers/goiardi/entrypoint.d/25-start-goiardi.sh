#!/bin/bash

echo "hostname=\"${EXTERNAL_IP%%/*}\"" >> /tmp/goiardi.conf.2
cat /etc/goiardi/goiardi.conf >> /tmp/goiardi.conf.2
mv /tmp/goiardi.conf.2 /etc/goiardi/goiardi.conf

for k in server.key server.crt admin.pem chef-validator.pem chef-webui.pem; do
    kv_get digitalrebar/private/goiardi/keys/$k >/etc/goiardi/$k && continue
    first_start=true
    break
done

if [[ $first_start ]]; then
    openssl genpkey -algorithm RSA -pkeyopt rsa_keygen_bits:2048 -out /etc/goiardi/server.key
    openssl req -new -key /etc/goiardi/server.key -out /etc/goiardi/server.csr -subj "/C=US/ST=Texas/L=Austin/O=RackN/OU=Goiardi/CN=neode.net"
    openssl x509 -req -days 365 -in /etc/goiardi/server.csr -signkey /etc/goiardi/server.key -out /etc/goiardi/server.crt
    rm /etc/goiardi/server.csr
fi
chmod 400 /etc/goiardi/server.key /etc/goiardi/server.crt /etc/goiardi/*.key

/go/bin/goiardi -c /etc/goiardi/goiardi.conf &

if [[ $first_start ]]; then
    count=0
    while [ ! -e /etc/goiardi/admin.pem ] ; do
        if [ $((count % 12)) -eq 0 ] ; then
            echo "Waiting for goiardi to start"
        fi
        sleep 5
        count=$((count+1))
    done
    for k in server.key server.crt admin.pem chef-validator.pem chef-webui.pem; do
        kv_put digitalrebar/private/goiardi/keys/$k </etc/goiardi/$k
    done
fi
