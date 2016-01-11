#!/bin/bash

cd /opt/digitalrebar/core

if [[ $DR_DEV ]]; then
	export RAILS_ENV=development
	export PUMA_CFG=puma-dev.cfg
	touch /tmp/development.txt
else
	export RAILS_ENV=production
	export PUMA_CFG=puma.cfg
fi
