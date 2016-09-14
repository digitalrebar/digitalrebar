#!/bin/bash

if [[ -f /tmp/.do_config ]]; then
    cd /opt/digitalrebar/core
    export BUILT_CFG_FILE=/tmp/final.json

    # This is a hack as well for now.
    cat > config/filters/admin-default.json <<EOF
{ 
  "name": "default",
  "priority": 50,
  "template": "{{node.name}}.$BASE_DOMAINNAME",
  "matcher": "net.category == \"admin\"",
  "service": "system"
}
EOF
    cp /home/rebar/.ssh/id_rsa.pub config/ssh_keys/admin-0.key

    ./rebar-build-json.rb > ${BUILT_CFG_FILE}
fi
