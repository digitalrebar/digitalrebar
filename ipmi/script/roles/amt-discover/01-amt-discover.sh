#!/bin/bash

if [[ -d /sys/module/mei_me ]] && grep -q mei_me < <(dmesg); then
    write_attribute 'amt/enable' true
fi
