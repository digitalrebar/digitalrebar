#!/bin/bash

export REBAR_ENDPOINT="https://${IP}:3000"

if [[ $DR_DEV ]]; then
    export RAILS_ENV=development
    export PUMA_CFG="puma-dev.cfg"
else
    export RAILS_ENV=production
    export PUMA_CFG="puma.cfg"
fi
