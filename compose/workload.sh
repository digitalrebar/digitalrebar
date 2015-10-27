#!/bin/bash
# Copyright 2015, RackN Inc

mkdir -p digitalrebar
cd digitalrebar
if [ ! -e $2 ] ; then
  git clone "https://github.com/${1}/${2}.git"
else
  cd $2
  git pull
  cd ..
fi
