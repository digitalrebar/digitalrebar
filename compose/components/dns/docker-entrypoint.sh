#!/bin/bash
set -e
set -x

consul agent --join consul --config-dir /etc/consul.d --data-dir /data &

HOSTNAME=`hostname`
IP=`ip addr show eth0 | grep inet | grep -v inet6 | awk '{ print $2 }' | awk -F/ '{ print $1 }'`

echo "IP.0=$IP" >> /etc/dns-mgmt-cert.conf
echo "DNS.0=$IP" >> /etc/dns-mgmt-cert.conf
echo "DNS.1=$HOSTNAME" >> /etc/dns-mgmt-cert.conf

openssl req -nodes -sha256 -x509 -newkey rsa:2048 \
   -keyout /etc/dns-mgmt-https-key.pem -out /etc/dns-mgmt-https-cert.pem -days 1001 \
   -subj "/C=US/ST=Denial/L=Anytown/O=Dis/CN=admin" -config /etc/dns-mgmt-cert.conf
chmod 600 /etc/dns-mgmt*

answer=""
# We need consul to converge on a leader.
# This can take a little time, so we ask for
# leader status.  The leader status returns
# nothing, a string that says no leader, or
# the leader IP:port pair.  The default port
# is 8300 for server communication.
while [[ $answer != *:8300* ]]; do
  sleep 1
  answer=`curl http://localhost:8500/v1/status/leader`
  echo "Waiting for consul leader: $answer"
done

# Make sure we set the token type
curl -X PUT -d 'BIND' http://127.0.0.1:8500/v1/kv/opencrowbar/private/dns/system/type?token=$CONSUL_M_ACL

# Access vars in consul for dns-mgmt
curl -X PUT -d 'admin' http://127.0.0.1:8500/v1/kv/opencrowbar/private/dns-mgmt/system/access_name?token=$CONSUL_M_ACL
curl -X PUT -d 'admin' http://127.0.0.1:8500/v1/kv/opencrowbar/private/dns-mgmt/system/access_password?$CONSUL_M_ACL
curl -X PUT --data-binary @/etc/dns-mgmt-https-cert.pem http://127.0.0.1:8500/v1/kv/opencrowbar/private/dns-mgmt/system/cert_pem?token=$CONSUL_M_ACL

/usr/local/bin/ocb-dns-mgmt &

ln -s /etc/init.d/bind9 /etc/init.d/named
service named start

while [ true ] ; do
  sleep 30
done
