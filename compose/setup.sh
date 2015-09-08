#!/bin/bash

mkdir -p components/rebar_api/digitalrebar
cd components/rebar_api/digitalrebar
if [ ! -e core ] ; then
  git clone https://github.com/digitalrebar/core.git
fi

