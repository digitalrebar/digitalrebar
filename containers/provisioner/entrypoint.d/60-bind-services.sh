#!/bin/bash

if ! rebar nodes roles "$HOSTNAME" |grep -q 'provisioner-base-images'; then
     rebar nodes bind "$HOSTNAME" to 'provisioner-database'
     rebar nodes bind "$HOSTNAME" to 'provisioner-base-images'
fi
