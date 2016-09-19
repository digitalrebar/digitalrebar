#!/bin/bash

make_service "logging" 514 '{"script": "pgrep rsyslogd","interval": "10s"}'
make_service "logging-relp" 2154 '{"script": "pgrep rsyslogd","interval": "10s"}'

consul reload

/usr/sbin/rsyslogd

bind_service logging-service
set_service_attrib logging-service logging_servers "{\"value\": [\"${EXTERNAL_IP%%/*}\"]}"
