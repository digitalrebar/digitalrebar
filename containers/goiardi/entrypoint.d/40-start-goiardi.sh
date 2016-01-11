#!/bin/bash

if [ "$FORWARDER_IP" == "" ] ; then
  sed -e "s/FILLMEIN/${EXTERNAL_IP%%/*}/" /root/goiardi-external.json > /etc/consul.d/goiardi.json
else
  cp /root/goiardi-internal.json /etc/consul.d/goiardi.json
fi
consul reload

echo "hostname=\"${EXTERNAL_IP%%/*}\"" > /tmp/goiardi.conf
cat /etc/goiardi/goiardi.conf >> /tmp/goiardi.conf
cp /tmp/goiardi.conf /etc/goiardi/goiardi.conf

if [[ ! -f /etc/goiardi/server.key ]]; then
    openssl ecparam -genkey -name secp521r1 -out /etc/goiardi/server.key
    openssl req -new -key /etc/goiardi/server.key -out /etc/goiardi/server.csr -subj "/C=US/ST=Texas/L=Austin/O=RackN/OU=Goiardi/CN=neode.net"
    openssl x509 -req -days 365 -in /etc/goiardi/server.csr -signkey /etc/goiardi/server.key -out /etc/goiardi/server.crt
    rm /etc/goiardi/server.csr
    chmod 400 /etc/goiardi/server.key /etc/goiardi/server.crt
fi

/go/bin/goiardi -c /etc/goiardi/goiardi.conf &

count=0
while [ ! -e /etc/goiardi/admin.pem ] ; do
  if [ $((count % 12)) -eq 0 ] ; then
      echo "Waiting for goiardi to start"
  fi
  sleep 5
  count=$((count+1))
done
