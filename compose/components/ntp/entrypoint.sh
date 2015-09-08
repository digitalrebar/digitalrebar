#!/bin/bash
set -e

consul agent --join consul --config-dir /etc/consul.d --data-dir /data &

# default behaviour is to launch squid
if [[ -z ${1} ]]; then
  echo "Starting ntpd..."
  exec /usr/sbin/ntpd -n -u ntp:ntp -p /var/run/ntpd.pid -g
else
  exec "$@"
fi
