#!/bin/bash

# Build UX components
(
    with_local_proxy
    cd /opt/digitalrebar-ux
    if [[ $REVPROXY_REBUILD_BOWER ]] ; then
        bower --allow-root install --config.interactive=false
        npm install cssnano-cli html-minifier uglify-js
        npm install -g n
        n stable
    fi
    ./build.sh
)
