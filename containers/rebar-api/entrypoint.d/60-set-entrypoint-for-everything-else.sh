#!/bin/bash

# Copy out stuff to data dir
cp /etc/profile.d/rebar* /node-data
echo "export REBAR_ENDPOINT=$REBAR_ENDPOINT" >> /node-data/rebar*
