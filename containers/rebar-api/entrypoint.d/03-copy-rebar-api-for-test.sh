#!/bin/bash

# Start up the code
# If we want to test a new rebar CLI, copy it in place.
if [[ -f /opt/digitalrebar/rebar && -x /opt/digitalrebar/rebar ]]; then
    cp /opt/digitalrebar/rebar /usr/local/bin
fi
