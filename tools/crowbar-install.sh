#!/bin/bash
#
# Copyright 2014, Greg Althaus
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#  http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

# Turn off firewalls.
chkconfig iptables off
service iptables stop

# Set selinux to permissive
setenforce 0
sed -i "s/SELINUX=.*/SELINUX=permissive/g" /etc/selinux/config

# Get release requested
if [[ $1 = '--develop' ]]; then
  shift
  RELEASE="develop"
  RELPATH="develop"
elif [[ $1 = '--master' ]]; then
  shift
  RELEASE="master"
  RELPATH="el6"
elif [[ $1 = '--release' ]]; then
  shift
  RELEASE="$1"
  RELPATH="release/$1"
  shift
else
  RELEASE="master"
  RELPATH="el6"
fi

# Setup repo
cd /etc/yum.repos.d
# Setup OCB repo
cat > ocb-$RELEASE-install.repo <<EOF
[ocb-$RELEASE]
name=$RELEASE repo for opencrowbar rpms
baseurl=http://opencrowbar.s3-website-us-east-1.amazonaws.com/$RELPATH
enabled=1
gpgcheck=0
type=none
autorefresh=1
keeppackages=1
EOF
cd -

# Clean up repos
yum clean all
yum makecache

# Install code
WITH_HARDWARE="NO"
if [[ $1 = '--without-hardware' ]]; then
    shift
    yum install -y opencrowbar-core
else
    yum install -y opencrowbar-hardware
    WITH_HARDWARE="YES"
fi

# Get the default installation OS.
mkdir -p /tftpboot/isos
cd /tftpboot/isos

WITH_DOWNLOAD="NO"
if [[ $1 = '--download-os' ]]; then
  shift
  WITH_DOWNLOAD="YES"
  wget http://mirrors.kernel.org/centos/7/isos/x86_64/CentOS-7-x86_64-Minimal-1503-01.iso
fi

cd -

if [[ $WITH_HARDWARE = 'YES' ]]; then
    mkdir -p /tftpboot/files/raid

    echo "Remember to populate /tftpboot/files/raid"
    echo "See: https://github.com/opencrowbar/hardware/tree/master/doc"
    echo ""
fi

if [[ $WITH_DOWNLOAD = 'NO' ]]; then
  echo "Remeber to populate /tftpboot/isos"
  echo "You will need to place a supported ISO image or images into this directory"
  echo "before running production.sh"
  echo "More details cab be found here:"
  echo "See: https://github.com/opencrowbar/core/tree/master/doc/deployment-guide/adding-operating-systems.md"
fi

