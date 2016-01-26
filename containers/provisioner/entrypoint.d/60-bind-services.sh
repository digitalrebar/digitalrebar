#!/bin/bash

if ! rebar nodes roles "$HOSTNAME" |grep -q 'provisioner-database'; then
     rebar nodes bind "$HOSTNAME" to 'provisioner-database'
fi
