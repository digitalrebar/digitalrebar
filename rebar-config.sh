#!/bin/bash
# Copyright 2014, Greg Althaus
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#  http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

set -e
date

. /etc/profile

hostname_re='([[:alnum:]]+\.){2,}[[:alnum:]]+'

check_hostname() {
    [[ $(hostname) =~ $hostname_re ]] && return
    echo "The hostname for the system must already be set to its proper name!"
    exit 1
}

check_hostname

[[ $FQDN ]] || export FQDN="$(hostname)"

DOMAINNAME=${FQDN#*.}
HOSTNAME=${FQDN%%.*}

if [[ $http_proxy && !$upstream_proxy ]] && ! pidof squid; then
    export upstream_proxy=$http_proxy
fi

# Update the provisioner server template to use whatever
# proxy the admin node should be using.
if [[ $upstream_proxy ]]; then
    rebar roles set proxy-server \
        attrib proxy-upstream_proxy \
        to "{\"value\": \"${upstream_proxy}\"}"
fi

set -e
set -x

unmanaged_net='
{
  "name": "unmanaged",
  "category": "unmanaged",
  "deployment": "system",
  "conduit": "?1g0",
  "configure": false,
  "ranges": [
    {
      "overlap": true,
      "name": "host",
      "first": "0.0.0.1/0",
      "last": "223.255.255.254/0"
    },
    {
      "overlap": true,
      "name": "host-v6",
      "first": "::1/0",
      "last": "ffff:ffff:ffff:ffff:ffff:ffff:ffff:fffe/0"
    }
  ]
}'

# Create the catch all network
rebar networks show 'unmananged' >/dev/null 2>&1 || rebar networks import "$unmanaged_net"

# Build a map of keys in the /root/.ssh/authorized_keys
# Record the machine key as well. -- THIS IS NOT GREAT
#
# This is after the join to make sure that the rebar-access
# role is added to the system deployment.
#
if [ -e /root/.ssh/authorized_keys ] ; then
    count=1
    rm -f /tmp/key_list
    echo "{ \"value\": {" > /tmp/key_list
    COMMA=""
    cat /root/.ssh/authorized_keys | while read line; do
        echo "$line" > config/ssh_keys/"admin-$count.key"
        echo "$COMMA \"admin-$count\": \"$line\"" >> /tmp/key_list
        count=`expr $count + 1`
        COMMA=","
    done
    echo "} }" >> /tmp/key_list
    rebar deployments bind system to rebar-access >/dev/null 2>&1 || true
    rebar deployments set system attrib rebar-access_keys to "`cat /tmp/key_list`"
    rebar deployments set system attrib rebar-machine_key to "{ \"value\": \"`cat /etc/rebar.install.key`\" }"
    rebar deployments commit system
    rm -rf /tmp/key_list
fi

# Always add the chef-service first
rebar nodes bind "system-phantom.internal.local" to "chef-service"

# Join the admin node into the rails app and make it manageable
./rebar-node.sh 127.0.0.1 'false'

# This may not be needed
echo "{ \"domain\": \"$DOMAINNAME\" }" > config/domain.json

# This is a hack as well for now.
echo "{ \
  \"name\": \"default\", \
  \"priority\": 50, \
  \"template\": \"{{node.name}}.$DOMAINNAME\", \
  \"matcher\": \"net.category == \\\"admin\\\"\", \
  \"service\": \"system\" \
}" > config/filters/admin-default.json

./rebar-build-json.rb > config/final.json

CONSUL_MACL=$(jq .acl_master_token </etc/consul.d/default.json | awk -F\" '{ print $2 }')
curl -X PUT --data-binary @config/final.json http://127.0.0.1:8500/v1/kv/digitalrebar/private/bootstrap?token=$CONSUL_MACL

