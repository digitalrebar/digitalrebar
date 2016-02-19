#!/bin/bash

# Load the initial barclamp
echo "Loading the core barclamp metadata"
/opt/digitalrebar/core/bin/barclamp_import /opt/digitalrebar/core

# Load the rest of the barclamps
while read bc; do
  echo "Loading barclamp metadata from $bc"
  /opt/digitalrebar/core/bin/barclamp_import "$bc" || :
done < <(find /opt/digitalrebar -name rebar.yml |grep -v '/core/')

# Create the system deployment
# We always need a system deployment
if ! rebar deployments show system; then
    rebar deployments create '{"name": "system", 
                               "description": "Created Automatically by System",
                               "system": true}' && \
        rebar deployments commit system || exit 1
fi
bind_service network-server
