#!/bin/bash

enable_amt=$(read_attribute 'rebar/hw_support/enable_amt')
if [[ $enable_amt == false ]]; then
    echo "Disabling amt because system-wide settings"
    write_attribute 'amt/enable' false
    exit 0
fi

if [[ -d /sys/module/mei_me ]] && grep -q mei_me < <(dmesg); then
    write_attribute 'amt/enable' true
else
    write_attribute 'amt/enable' false
fi
