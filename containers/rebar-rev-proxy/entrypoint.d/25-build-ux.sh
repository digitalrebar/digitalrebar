#!/bin/bash

# Build UX components
(
    with_local_proxy
    cd /opt/digitalrebar-ux
    if [[ ! -d node_modules ]] ; then
        npm install --dev
    fi
    [[ -f .using_prebuilt ]] && brunch build
)
