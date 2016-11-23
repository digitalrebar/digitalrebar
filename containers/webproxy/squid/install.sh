#!/bin/bash

set -e

apt-get update \
    && apt-get -y install libecap3 devscripts build-essential \
    fakeroot debhelper dh-autoreconf cdbs squid3 squid-langpack \
    nettle-dev libgnutls28-dev libssl-dev libdbi-perl wget \
    libldap2-dev libpam0g-dev libdb-dev libsasl2-dev libcppunit-dev \
    libkrb5-dev comerr-dev libcap2-dev libecap3-dev libexpat1-dev \
    libxml2-dev libnetfilter-conntrack-dev dh-apparmor bash

wget http://archive.ubuntu.com/ubuntu/pool/main/s/squid3/squid3_3.5.12-1ubuntu8.dsc
wget http://archive.ubuntu.com/ubuntu/pool/main/s/squid3/squid3_3.5.12.orig.tar.gz
wget http://archive.ubuntu.com/ubuntu/pool/main/s/squid3/squid3_3.5.12-1ubuntu8.debian.tar.xz

dpkg-source -x squid3_3.5.12-1ubuntu8.dsc

patch squid3-3.5.12/debian/rules < /tmp/rules.patch

cd squid3-3.5.12 && dpkg-buildpackage -rfakeroot -b

cp *.deb /debs
