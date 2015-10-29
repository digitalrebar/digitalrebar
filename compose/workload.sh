#!/bin/bash
# Copyright 2015, RackN Inc

set -e

mkdir -p digitalrebar
cd digitalrebar

USERPW=$RACKN_USER
if [ "$RACKN_PASSWORD" != "" ] ; then
    USERPW="${USERPW}:${RACKN_PASSWORD}"
fi
if [ "$USERPW" != "" ] ; then
    USERPW="${USERPW}@"
fi

if [ ! -e $2 ] ; then
  git clone "https://${USERPW}github.com/${1}/${2}.git"
else
  cd $2
  git pull
  cd ..
fi
