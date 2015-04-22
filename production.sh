#!/bin/bash
# Copyright 2014, Dell
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

date
export RAILS_ENV=production

# Hack to clean out docker container for now.
rm -rf /etc/yum.repos.d/crowbar-open*

cd /opt/opencrowbar/core
. ./bootstrap.sh
set -e
set -o pipefail
if [[ $http_proxy && !$upstream_proxy ]] && ! pidof squid; then
    export upstream_proxy=$http_proxy
fi

hostname_re='([[:alnum:]]+\.){2,}[[:alnum:]]+'
FQDN=$(hostname)
if [[ $1 ]]; then
    if [[ $1 != $FQDN ]]; then
        if ! [[ $1 =~ $hostname_re ]]; then
            echo "Cannot set system hostname to $1, it is not a valid hostname!"
            exit 1
        fi
        if [[ -f /.dockerenv ]]; then
            FQDN="$(hostname -s).${1#*.}"
        else
            FQDN="$1"
            # Fix up the localhost address mapping.
            sed -i -e "s/\(127\.0\.0\.1.*\)/127.0.0.1 $FQDN $HOSTNAME localhost.localdomain localhost/" /etc/hosts
            sed -i -e "s/\(127\.0\.1\.1.*\)/127.0.1.1 $FQDN $HOSTNAME localhost.localdomain localhost/" /etc/hosts
            # Fix Ubuntu/Debian Hostname
            echo "$FQDN" > /etc/hostname
        fi
        echo "Changing system hostname from $(hostname) to $FQDN"
        hostname $FQDN
    fi
elif ! [[ $FQDN =~ $hostname_re ]]; then
    echo "$FQDN is not a valid FQDN"
    echo "Please pass one as the first parameter to this script so we can set it."
    exit 1
fi
 
# Fix CentOs/RedHat Hostname
if [ -f /etc/sysconfig/network ] ; then
  sed -i -e "s/HOSTNAME=.*/HOSTNAME=$FQDN/" /etc/sysconfig/network
fi

# Set domainname (for dns)
echo "${FQDN#*.}" > /etc/domainname

export FQDN

mkdir -p /var/log/crowbar
for stage in boot consul database core config; do
    "./crowbar-${stage}.sh" 2>&1 |tee "/var/log/crowbar/install-${stage}.log"
done
