#!/bin/bash

baseurl="http://127.0.0.1:8500/v1/kv/digitalrebar/private/database/goiardi"
token="?token=$CONSUL_M_ACL"
pass=`curl ${baseurl}/password${token}`
while [ "$pass" == "" ] ; do
  sleep 5
  pass=`curl ${baseurl}/password${token}`
done
echo $pass | jq -r .[0].Value | base64 -d > ~/.pgpass
chmod 600 ~/.pgpass
rm -f ~/.pgpass
