#!/bin/bash

# Copy out stuff to data dir
cp /etc/profile.d/rebar* /etc/rebar-data
echo "export REBAR_ENDPOINT=$REBAR_ENDPOINT" >> /etc/rebar-data/rebar*
echo "export EXTERNAL_REBAR_ENDPOINT=https://${EXTERNAL_IP%%/*}:3000" >> /etc/rebar-data/rebar*
