#!/bin/bash

# Build UX components
cd /opt/digitalrebar-ux
bower --allow-root install --config.interactive=false
./build.sh
cd -

