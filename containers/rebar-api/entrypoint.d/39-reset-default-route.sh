#!/bin/bash
if [[ $forwarder ]] ; then
    ip route del default
    ip route add default via $forwarder
fi
