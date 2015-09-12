#!/bin/bash
# Copyright 2015, RackN Inc

mkdir -p components/rebar_api/digitalrebar
cd components/rebar_api/digitalrebar
if [ ! -e $2 ] ; then
  git clone "https://github.com/${1}/${2}.git"
  ln -s "components/rebar_api/digitalrebar/${2}" $2
else
  cd $2
  git pull
  cd ..
fi
