#!/bin/bash

(
    while true; do
        logrotate /etc/logrotate.conf
        sleep 1d
    done
) &
