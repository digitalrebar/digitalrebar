#!/bin/bash
# Set up vars
echo "EXTERNAL_IP=$EXTERNAL_IP" >> /etc/environment

echo "export EXTERNAL_IP=$EXTERNAL_IP" >> /etc/profile.d/container.sh
chmod +x /etc/profile.d/container.sh
