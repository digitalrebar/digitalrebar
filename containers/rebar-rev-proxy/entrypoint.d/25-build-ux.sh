#!/bin/bash

# Build UX components
cd /opt/digitalrebar-ux
if [[ $REVPROXY_REBUILD_BOWER ]] ; then
  bower --allow-root install --config.interactive=false
fi
./build.sh
cd -

