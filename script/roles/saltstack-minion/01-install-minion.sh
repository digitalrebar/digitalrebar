#!/bin/bash

build_config_file() {
    echo "id: $3" > "$1"
    echo "master:" >> "$1"
    IFS="," read -a array <<< "$2"
    for element in "${array[@]}"
    do
      echo " - $element" >> "$1"
    done
}

set restart=0

if has_attribute "saltstack/minion/public_key"; then
    TMP_KEY_FILE="/tmp/tt.$$.pub.key"
    read_attribute_file_content "saltstack/minion/public_key" "$TMP_KEY_FILE"
    if ! diff -q "$TMP_KEY_FILE" /etc/salt/pki/minion/minion.pub 2>&1 >/dev/null; then
        restart=1
        mkdir -p /etc/salt/pki/minion
        cp "$TMP_KEY_FILE" /etc/salt/pki/minion/minion.pub
    fi
    rm -f "$TMP_KEY_FILE"
fi

if has_attribute "saltstack/minion/private_key"; then
    TMP_KEY_FILE="/tmp/tt.$$.priv.key"
    read_attribute_file_content "saltstack/minion/private_key" "$TMP_KEY_FILE"
    if ! diff -q "$TMP_KEY_FILE" /etc/salt/pki/minion/minion.pem 2>&1 >/dev/null; then
        restart=1
        mkdir -p /etc/salt/pki/minion
        cp "$TMP_KEY_FILE" /etc/salt/pki/minion/minion.pem
    fi
    rm -f "$TMP_KEY_FILE"
fi

# Install salt-minion
if ! which salt-minion; then
    if [[ -f /etc/redhat-release || -f /etc/centos-release ]]; then
        yum -y makecache
        yum install -y salt-minion
        chkconfig salt-minion on
        service salt-minion stop
    elif [[ -d /etc/apt ]]; then
        apt-get -y --force-yes install software-properties-common
        add-apt-repository ppa:saltstack/salt
        apt-get -y update
        apt-get -y --force-yes install salt-minion
    elif [[ -f /etc/SuSE-release ]]; then
        zypper install -y -l salt salt-minion
        systemctl enable salt-minion.service
    else
        die "Staged on to unknown OS media!"
    fi
fi

# Figure master to config
ip=$(read_attribute "saltstack/master/ip")
name=$(read_attribute "saltstack/minion/name")

if [[ "$name" == "" ]]; then
    die "Unspecified name for SaltStack minion"
fi

if [[ "$ip" == "" ]]; then
    die "Unspecified ip for SaltStack master"
fi

# Build and Update config file
TMP_CONFIG_FILE="/tmp/cb.scr.minion.$$"
build_config_file "$TMP_CONFIG_FILE" "$ip" $name
if ! diff -q "$TMP_CONFIG_FILE" /etc/salt/minion 2>&1 >/dev/null; then
    cp "$TMP_CONFIG_FILE" /etc/salt/minion
    restart=1
fi
rm -f "$TMP_CONFIG_FILE"

if [[ $restart -eq 1 ]]; then
    if [[ -f /etc/redhat-release || -f /etc/centos-release ]]; then
        service salt-minion stop
        service salt-minion start
    elif [[ -d /etc/apt ]]; then
        service salt-minion restart
    elif [[ -f /etc/SuSE-release ]]; then
        systemctl stop salt-minion.service
        systemctl start salt-minion.service
    else
        die "Staged on to unknown OS media!"
    fi
fi

# Wait for files
while [[ ! -f /etc/salt/pki/minion/minion.pub ]]; do
    sleep 1
done


# Save the cred files
write_attribute_file_content "saltstack/minion/public_key" /etc/salt/pki/minion/minion.pub
write_attribute_file_content "saltstack/minion/private_key" /etc/salt/pki/minion/minion.pem

