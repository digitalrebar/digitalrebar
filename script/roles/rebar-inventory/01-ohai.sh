#!/bin/bash

if [[ -f /etc/os-release ]]; then
  . /etc/os-release
fi

# If ohai is tool old, remove it
if  which ohai; then
    matcher='^Ohai: 6'
    if [[ "$(ohai --version)" =~ $matcher ]] ; then
        if [[ -d /etc/apt ]]; then
            apt-get purge -y ohai
            hash -r
        else
            die "Need to remove ohai"
        fi
    fi
fi

# Add a good version
if ! which ohai; then
    if [[ -f /etc/redhat-release || -f /etc/centos-release ]]; then
        yum install -y ruby-devel gcc
        matcher="^ruby 1.9"
        if [[ $(ruby --version) =~ $matcher ]] ; then
            gem install ohai -v 7.4.1
        else
            gem install ffi-yajl -v 2.2.3
            gem install ohai
        fi
        hash -r
    elif [[ -d /etc/apt ]]; then
        apt-get install -y ruby-dev build-essential
        matcher="^ruby 1.9"
        if [[ $(ruby --version) =~ $matcher ]] ; then
            gem install ohai -v 7.4.1
        else
            gem install ffi-yajl -v 2.2.3
            gem install ohai
        fi
        hash -r
    elif [[ -f /etc/SuSE-release ]]; then
        zypper install -y -l ohai
    else
        die "Staged on to unknown OS media!"
    fi
fi

ohai --directory $TMPDIR/rebar-inventory/plugin > $TMPDIR/attrs/ohai.json

