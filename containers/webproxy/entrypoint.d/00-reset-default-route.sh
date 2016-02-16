#!/bin/bash

if [[ $FORWARDER_IP && $forwarder ]]; then
    ip route del default
    ip route add default via $forwarder
fi

