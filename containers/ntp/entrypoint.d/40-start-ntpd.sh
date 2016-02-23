#!/bin/bash

#
# This really on works if forwarder is off
#
if [[ ! $FORWARDER_IP ]] ; then
    bind_service ntp-service
    make_service "ntp" 123 '{"script": "ntpdate -q 127.0.0.1 2>&1 >/dev/null","interval": "10s"}'
    while true; do
        /usr/sbin/ntpd -n -u ntp:ntp -p /var/run/ntpd.pid -g -l /dev/stdout
        echo "NTP DIED!!!! ... Restarting"
    done
fi

