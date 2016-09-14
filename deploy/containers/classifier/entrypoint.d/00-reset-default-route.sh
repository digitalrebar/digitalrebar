#!/bin/bash

if [[ $FORWARDER_IP && $forwarder ]]; then
    ip route del default
    ip route add default via $forwarder
fi

# Set up vars
echo "FORWARDER_IP=$FORWARDER_IP" >> /etc/environment
echo "EXTERNAL_IP=$EXTERNAL_IP" >> /etc/environment

echo "export FORWARDER_IP=$FORWARDER_IP" >> /etc/profile.d/container.sh
echo "export EXTERNAL_IP=$EXTERNAL_IP" >> /etc/profile.d/container.sh
chmod +x /etc/profile.d/container.sh
