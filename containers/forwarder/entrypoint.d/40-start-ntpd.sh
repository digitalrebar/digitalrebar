#!/bin/bash

if [[ $RUN_NTP ]] ; then
    
    (unset FORWARDER_IP; make_service "ntp" "123" '{"script": "ntpdate -q 127.0.0.1 2>&1 >/dev/null","interval": "10s"}')
    consul reload

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
    bind_service ntp-service
    /usr/sbin/ntpd -u ntp:ntp -p /var/run/ntpd.pid -g
fi
