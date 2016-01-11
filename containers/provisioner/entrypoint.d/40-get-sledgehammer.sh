#!/bin/bash

PROV_TFTPROOT="/tftpboot"
PROV_WEBPORT="8091"

if [ "$PROV_SLEDGEHAMMER_SIG" == "" ] ; then
  echo "Sledgehammer Hash not specified"
  exit 1
fi

if [ "$PROV_SLEDGEHAMMER_URL" == "" ] ; then
  echo "Sledgehammer URL not specified"
  exit 1
fi

# Get sledgehammer
SS=$PROV_SLEDGEHAMMER_SIG
SS_URL=$PROV_SLEDGEHAMMER_URL/$SS
SS_DIR=$PROV_TFTPROOT/sledgehammer/$SS
mkdir -p $SS_DIR
cd $SS_DIR
if [ ! -e $SS_DIR/sha1sums ] ; then
  curl -L -f -O $SS_URL/initrd0.img
  curl -L -f -O $SS_URL/vmlinuz0
  curl -L -f -O $SS_URL/sha1sums
  sha1sum -c sha1sums
fi
cd -

# Make it the discovery image
mkdir -p $PROV_TFTPROOT/discovery
cd $PROV_TFTPROOT/discovery
rm -f $PROV_TFTPROOT/discovery/initrd0.img
#ln -s ../sledgehammer/$SS/initrd0.img initrd0.img
cp ../sledgehammer/$SS/initrd0.img initrd0.img
rm -f $PROV_TFTPROOT/discovery/vmlinuz0
#ln -s ../sledgehammer/$SS/vmlinuz0 vmlinuz0
cp ../sledgehammer/$SS/vmlinuz0 vmlinuz0
cd -

