#!/bin/bash

if ! which puppet; then
    gem install puppet
    puppet resource group puppet ensure=present
    puppet resource user puppet ensure=present gid=puppet shell='/sbin/nologin'
fi

node_name=$(read_attribute "crowbar/puppet-agent-sa/name")
echo "Puppet node: $node_name"
cat > "/etc/puppet/puppet.conf" <<EOF
[main]
certname = $node_name
logdir=/var/log/puppet
vardir=/var/lib/puppet
ssldir=/var/lib/puppet/ssl
rundir=/var/run/puppet
factpath=\$vardir/lib/facter
templatedir=\$confdir/templates
EOF