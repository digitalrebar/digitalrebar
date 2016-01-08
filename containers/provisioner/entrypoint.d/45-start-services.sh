#!/bin/bash

# Start tftpd
service tftpd-hpa restart

# Start web server
/usr/local/bin/sws --listen=:$PROV_WEBPORT --site=$PROV_TFTPROOT </dev/null 2>&1 1>/var/log/sws.log &
