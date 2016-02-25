#!/bin/bash

#
# This really on works if forwarder is off
#
if [[ ! $FORWARDER_IP ]] ; then
    bind_service ntp-service
    make_service "ntp" 123 '{"script": "ntpdate -q 127.0.0.1 2>&1 >/dev/null","interval": "10s"}'

    if [[ ! $EXTERNAL_NTP_SERVER ]] ; then
        cat >> /etc/ntp.conf <<EOF
server 127.127.1.0
fudge 127.127.1.0 stratum 1
EOF
    else
        cat >> /etc/ntp.conf <<EOF
server $EXTERNAL_NTP_SERVER
EOF
    fi

    while true; do
        /usr/sbin/ntpd -n -u ntp:ntp -p /var/run/ntpd.pid -g -l /dev/stdout
        echo "NTP DIED!!!! ... Restarting"
    done
fi

