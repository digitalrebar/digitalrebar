#!/bin/bash

# Make sure rebar user and ssh are in place
if [[ ! -f /home/rebar/.ssh/id_rsa ]] ; then
    su -l -c 'ssh-keygen -q -b 2048 -P "" -f /home/rebar/.ssh/id_rsa' rebar
fi
cd /opt/digitalrebar/core
