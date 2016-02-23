#!/bin/bash
# Copyright 2015, RackN Inc

set -e

mkdir -p digitalrebar
cd digitalrebar

if [ ! -e "~/.netrc" ] ; then
	USERPW=$RACKN_USER
	if [ "$RACKN_PASSWORD" != "" ] ; then
	    USERPW="${USERPW}:${RACKN_PASSWORD}"
	fi
	if [ "$USERPW" != "" ] ; then
	    USERPW="${USERPW}@"
	fi
fi

if [ ! -e $2 ] ; then
  git clone "https://${USERPW}github.com/${1}/${2}.git"
  if [[ $DR_TAG && $DR_TAG != "latest" ]] ; then
      cd $2
      git checkout ${DR_TAG}
      cd ..
  fi
else
  cd $2
  git pull
  cd ..
fi
