#!/bin/bash

# Build UX components
cd /opt/digitalrebar-ux
bower --allow-root install --config.interactive=false
npm install cssnano-cli html-minifier uglify-js
npm install -g n
n stable
./build.sh
cd -

