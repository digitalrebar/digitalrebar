#!/bin/bash

# Start tftpd
/usr/sbin/in.tftpd --listen --user root --address 0.0.0.0:69 --secure /tftpboot &
# Start web server
/usr/local/bin/sws --listen=:8091 --site=/tftpboot </dev/null 2>&1 1>/var/log/sws.log &
