#!/bin/bash

if [[ $RUN_NTP ]] ; then
    
    if [[ ! $EXTERNAL_NTP_SERVERS ]] ; then
        cat >> /etc/ntp.conf <<EOF
server 127.127.1.0
fudge 127.127.1.0 stratum 1
EOF
        NTP_SERVER_IP=${EXTERNAL_IP}
    else
	IFS=', ' read -r -a external_server_array <<< "$EXTERNAL_NTP_SERVERS"
	for element in "${external_server_array[@]}"
	do
	    echo "server $element" >> /etc/ntp.conf
        done
        NTP_SERVER_IP="${external_server_array[0]}/24"
    fi

    TEST_IP="${NTP_SERVER_IP%%/*}"
    (unset FORWARDER_IP; make_service "ntp" "123" "{\"script\": \"ntpdate -q $TEST_IP 2>&1 >/dev/null\",\"interval\": \"10s\"}')
    bind_service ntp-service

    if [[ $NTP_RUN_PROXY == YES ]] ; then
        /usr/sbin/ntpd -u ntp:ntp -p /var/run/ntpd.pid -g
    fi
fi
