#!/bin/bash
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

set -e
set -x
date

# setup & load env info
if [[ ! -f /etc/profile.d/rebar.sh ]]; then
    cat > /etc/profile.d/rebar.sh <<EOF
if ! fgrep -q '/opt/digitalrebar/core/bin' < <(echo \$PATH); then
    export PATH=\$PATH:/opt/digitalrebar/core/bin
fi
EOF
fi
. ./bootstrap.sh

if [[ ! $RAILS_ENV ]]; then
    echo "RAILS_ENV not set!"
    exit 1
fi

check_hostname

. /etc/profile
./setup/00-rebar-rake-tasks.install && \
    ./setup/01-rebar-start.install && \
    ./setup/02-make-machine-key.install || {
        echo "Failed to bootstrap the Rebar UI"
        exit 1
    }

[[ -f /etc/rebar.install.key ]] || {
    echo "Cannot find the Rebar install key!"
    exit 1
}

read -r -s REBAR_KEY < /etc/rebar.install.key
export REBAR_KEY

# Load the initial barclamp
echo "Loading the core barclamp metadata"
/opt/digitalrebar/core/bin/barclamp_import /opt/digitalrebar/core

# Load the rest of the barclamps
while read bc; do
    echo "Loading barclamp metadata from $bc"
    /opt/digitalrebar/core/bin/barclamp_import "$bc"
done < <(find /opt/digitalrebar -name rebar.yml |grep -v '/core/')

# Add consul to the default deployment, and make sure it uses the same
# acl master token and encryption key as the current running consul.
rebar deployments bind system to consul

for k in acl_master_token encrypt datacenter domain acl_datacenter \
                          acl_default_policy acl_down_policy config_dir; do
    v="$(jq ".${k}" </etc/consul.d/default.json)"
    [[ $v = null ]] && continue
    rebar deployments set system attrib "consul-${k//_/-}" to "{\"value\": $v}"
done
rebar deployments commit system

