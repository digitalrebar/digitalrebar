#!/bin/bash

# Build UX components
(
    with_local_proxy
    cd /opt/digitalrebar-ux
    if [[ ! -d bower_components ]]; then
        bower --allow-root install --config.interactive=false
        npm install cssnano-cli html-minifier uglify-js
    fi
    [[ -f .using_prebuilt ]] || ./build.sh
)
