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

set -e
date 
export RAILS_ENV=development
touch /tmp/development.txt

# developers may not want TMUX, give them a hint
if [[ $TMUX ]]; then
  echo 'Using TMUX > "export TMUX=false" to disable.'
fi

cd /opt/opencrowbar/core

# cleanup logs
if [ -f rails/log/development.log ]; then
  rm rails/log/development.log
fi

# bootstrap
. ./bootstrap.sh

if [[ $http_proxy && !$upstream_proxy ]] && ! pidof squid; then
    export upstream_proxy=$http_proxy
fi

# for dev environment, we force the FQDN
FQDN="devadmin.opencrowbar.org"
hostname $FQDN

# Fix CentOs/RedHat Hostname
if [ -f /etc/sysconfig/network ] ; then
  sed -i -e "s/HOSTNAME=.*/HOSTNAME=$FQDN/" /etc/sysconfig/network
fi
# Set domainname (for dns)
echo "${FQDN#*.}" > /etc/domainname
export FQDN

./crowbar-boot.sh
./crowbar-consul.sh
./crowbar-database.sh
./crowbar-core.sh "$RAILS_ENV"

# we don't run crowbar-config but we do need to do some things from there
	# Add consul to the default deployment, and make sure it uses the same
	# acl master token and encryption key as the current running consul.
./bin/crowbar deployments bind system to consul

for k in acl_master_token encrypt datacenter domain acl_datacenter \
                          acl_default_policy acl_down_policy config_dir; do
    v="$(jq ".${k}" </etc/consul.d/default.json)"
    [[ $v = null ]] && continue
    ./bin/crowbar deployments set system attrib "consul-${k//_/-}" to "{\"value\": $v}"
done

./bin/crowbar deployments commit system

# Make sure that Crowbar is running with the proper environment variables
service crowbar stop
service crowbar start
