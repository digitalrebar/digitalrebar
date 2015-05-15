#!/bin/bash

# Don't run in docker.
if [[ -f /.dockerenv ]]; then
    exit 0
fi

if [[ -f /etc/os-release ]]; then
  . /etc/os-release
fi

if ! which sysdig; then
    if [[ -f /etc/redhat-release || -f /etc/centos-release ]]; then
        yum -y makecache
        yum install -y kernel-devel-$(uname -r)
        yum install -y sysdig
    elif [[ -d /etc/apt ]]; then
        apt-get -y update
        apt-get -y install --force-yes linux-headers-$(uname -r)
        apt-get -y install --force-yes sysdig
    else
        die "Staged on to unknown OS media!"
    fi
fi

