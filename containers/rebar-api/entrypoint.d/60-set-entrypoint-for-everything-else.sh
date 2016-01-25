#!/bin/bash

# Copy out stuff to data dir
cp /etc/profile.d/rebar* /node-data
echo "export REBAR_ENDPOINT=$REBAR_ENDPOINT" >> /node-data/rebar*
echo "export EXTERNAL_REBAR_ENDPOINT=https://${EXTERNAL_IP%%/*}:3000" >> /node-data/rebar*
