#!/bin/bash

#
# This really on works if forwarder is off
#
if [ "$FORWARDER_IP" == "" ] ; then
    sed -e "s/FILLMEIN/${EXTERNAL_IP%%/*}/" /root/ntp.json > /etc/consul.d/ntp.json
    consul reload
    while true; do
        /usr/sbin/ntpd -n -u ntp:ntp -p /var/run/ntpd.pid -g -l /dev/stdout
        echo "NTP DIED!!!! ... Restarting"
    done
else
    while true; do
        sleep 360
    done
fi

