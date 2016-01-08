#!/bin/bash

mkdir -p /root/.ssh/
if [[ ! -f /root/.ssh/id_rsa ]] ; then
    ssh-keygen -q -b 2048 -P "" -f /root/.ssh/id_rsa
fi
chmod 700 /root/.ssh
chmod 600 /root/.ssh/*

