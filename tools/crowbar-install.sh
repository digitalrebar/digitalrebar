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

# Setup repos
cd /etc/yum.repos.d
if [[ $1 = '--develop' ]]; then
  shift
  # Setup OCB repo
  cat > ocb-develop-install.repo <<EOF
[ocb-develop]
name=develop repo for opencrowbar rpms
baseurl=http://opencrowbar.s3-website-us-east-1.amazonaws.com/develop
enabled=1
gpgcheck=0
type=none
autorefresh=1
keeppackages=1
EOF
else
  # Setup OCB repo
  cat > ocb-install.repo <<EOF
[ocb]
name=repo for opencrowbar rpms
baseurl=http://opencrowbar.s3-website-us-east-1.amazonaws.com/el6
enabled=1
gpgcheck=0
type=none
autorefresh=1
keeppackages=1
EOF
fi

# Setup OCB Ruby repo
cat > ocb-ruby.repo <<EOF
[ocb-ruby]
name=OCB Ruby for 6
baseurl=http://opencrowbar.s3-website-us-east-1.amazonaws.com/ruby
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
  wget http://mirrors.kernel.org/centos/7/isos/x86_64/CentOS-7.0-1406-x86_64-DVD.iso
fi

cd -

if [[ $WITH_HARDWARE = 'YES' ]]; then
    mkdir -p /tftpboot/files/raid

    echo "Remember to populate /tftpboot/files/raid"
    echo "See: https://github.com/opencrowbar/hardware/tree/master/doc"
    echo ""
    echo "Files can be pulled from these links after accepting licenses."
    echo "  http://www.lsi.com/downloads/Public/Host%20Bus%20Adapters/Host%20Bus%20Adapters%20Common%20Files/SAS_SATA_6G_P19/SAS2IRCU_P19.zip"
    echo "  http://www.lsi.com/downloads/Public/RAID%20Controllers/RAID%20Controllers%20Common%20Files/8.07.14_MegaCLI.zip"
    echo ""
fi

if [[ $WITH_DOWNLOAD = 'NO' ]]; then
  echo "Remeber to populate /tftpboot/isos"
  echo "You will need to place a supported ISO image or images into this directory"
  echo "before running production.sh"
  echo "More details cab be found here:"
  echo "See: https://github.com/opencrowbar/core/tree/master/doc/deployment-guide/adding-operating-systems.md"
fi

