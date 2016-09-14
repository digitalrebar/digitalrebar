#!/bin/bash

# Just make sure all our bundled gems are properly configured locally.
su -l -c 'cd /opt/digitalrebar/core/rails; bundle install --local --path /var/cache/rebar/gems --standalone --binstubs /var/cache/rebar/bin' rebar
