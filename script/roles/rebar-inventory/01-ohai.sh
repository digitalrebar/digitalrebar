#!/bin/bash

if [[ -f /etc/os-release ]]; then
  . /etc/os-release
fi

if ! which ohai; then
    if [[ -f /etc/redhat-release || -f /etc/centos-release ]]; then
        yum install -y ruby-devel gcc
        gem install ohai
    elif [[ -d /etc/apt ]]; then
        apt-get install -y ohai
    elif [[ -f /etc/SuSE-release ]]; then
        zypper install -y -l ohai
    else
        die "Staged on to unknown OS media!"
    fi
fi

ohai --directory $TMPDIR/rebar-inventory/plugin > $TMPDIR/attrs/ohai.json

