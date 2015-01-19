#!/bin/bash

set restart=0

if has_attribute "saltstack/master/public_key"; then
    TMP_KEY_FILE="/tmp/tt.$$.pub.key"
    read_attribute_file_content "saltstack/master/public_key" "$TMP_KEY_FILE"

    if ! diff -q "$TMP_KEY_FILE" /etc/salt/pki/master/master.pub 2>&1 >/dev/null; then
        restart=1
        mkdir -p /etc/salt/pki/master
        cp "$TMP_KEY_FILE" /etc/salt/pki/master/master.pub
    fi
    rm -f "$TMP_KEY_FILE"
fi

if has_attribute "saltstack/master/private_key"; then
    TMP_KEY_FILE="/tmp/tt.$$.priv.key"
    read_attribute_file_content "saltstack/master/private_key" "$TMP_KEY_FILE"

    if ! diff -q "$TMP_KEY_FILE" /etc/salt/pki/master/master.pem 2>&1 >/dev/null; then
        restart=1
        mkdir -p /etc/salt/pki/master
        cp "$TMP_KEY_FILE" /etc/salt/pki/master/master.pem
    fi
    rm -f "$TMP_KEY_FILE"
fi

if ! which salt-master; then
    if [[ -f /etc/redhat-release || -f /etc/centos-release ]]; then
        yum install -y epel-release  # tested to work on cent 6.5, 6.6, and 7.0 as of 01/19/2015
        yum -y makecache
        yum install -y GitPython
        yum install -y salt-master
        chkconfig salt-master on
        restart=1
    elif [[ -d /etc/apt ]]; then
        apt-get -y --force-yes install software-properties-common
        add-apt-repository ppa:saltstack/salt
        apt-get -y update
        apt-get -y --force-yes install python-git
        apt-get -y --force-yes install salt-master
    elif [[ -f /etc/SuSE-release ]]; then
        zypper install -y -l salt python-git
        zypper install -y -l salt salt-master
        systemctl enable salt-master.service
        restart=1
    else
        die "Staged on to unknown OS media!"
    fi
fi

if [[ $restart -eq 1 ]]; then
    if [[ -f /etc/redhat-release || -f /etc/centos-release ]]; then
        service salt-master stop
        service salt-master start
    elif [[ -d /etc/apt ]]; then
        service salt-master restart
    elif [[ -f /etc/SuSE-release ]]; then
        systemctl stop salt-master.service
        systemctl start salt-master.service
    else
        die "Staged on to unknown OS media!"
    fi
fi

# Save the cred files
write_attribute_file_content "saltstack/master/public_key" /etc/salt/pki/master/master.pub
write_attribute_file_content "saltstack/master/private_key" /etc/salt/pki/master/master.pem

# Clean out minions
minions=$(ls /etc/salt/pki/master/minions)
if [[ "${minions}" != "" ]]; then
    for name in $minions
    do
        if [[ ! $(has_attribute "saltstack/master/keys/${name}") ]]; then
            rm -f "/etc/salt/pki/master/minions/${name}"
        fi
    done
fi

# Put minions in place.
key_names=$(get_keys "saltstack/master/keys")
if [[ "${key_names}" != "" ]]; then
    for name in $key_names
    do
      read_attribute_file_content "saltstack/master/keys/${name}" "/etc/salt/pki/master/minions/${name}"
    done
fi

# Clean up the pending minions
rm -rf /etc/salt/pki/master/minions_pre/*

