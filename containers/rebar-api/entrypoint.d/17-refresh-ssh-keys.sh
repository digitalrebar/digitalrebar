#!/bin/bash

# Make sure rebar user and ssh are in place
ssh_priv=''

if ! ssh_priv=$(kv_get digitalrebar/private/api/keys/ssh_priv); then
    su -l -c 'ssh-keygen -q -b 2048 -P "" -f /home/rebar/.ssh/id_rsa' rebar
    su -l -c 'cat /home/rebar/.ssh/id_rsa' |kv_put digitalrebar/private/api/keys/ssh_priv
    su -l -c 'cat /home/rebar/.ssh/id_rsa.pub' |kv_put digitalrebar/private/api/keys/ssh_pub
else
    mkdir -p rebar_ssh
    printf '%s' "$ssh_priv" >rebar_ssh/id_rsa
    kv_get digitalrebar/private/api/keys/ssh_pub >rebar_ssh/id_rsa.pub
    mkdir -p /home/rebar/.ssh/
    cp rebar_ssh/id* /home/rebar/.ssh/
    chown -R rebar:rebar /home/rebar/.ssh/
    chmod 0400 /home/rebar/.ssh/id_rsa
fi
