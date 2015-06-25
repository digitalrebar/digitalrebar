#!/bin/bash
# Copyright 2015, Greg Althaus
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

. /etc/profile

hostname_re='([[:alnum:]]+\.){2,}[[:alnum:]]+'

check_hostname() {
    [[ $(hostname) =~ $hostname_re ]] && return
    echo "The hostname for the system must already be set to its proper name!"
    exit 1
}

check_hostname

[[ $FQDN ]] || export FQDN="$(hostname)"

DOMAINNAME=${FQDN#*.}
HOSTNAME=${FQDN%%.*}

set -x

admin_net_name='the_admin'

# Figure out what IP addresses we should have, and add them.
# If the above adds an address, we need to make sure it starts on the node.
netline=$(crowbar nodes addresses "$FQDN" on $admin_net_name)
nets=(${netline//,/ })
for net in "${nets[@]}"; do
    [[ $net =~ $ip_re ]] || continue
    net=${BASH_REMATCH[1]}
    # Make this more complicated and exact later.
    ip addr add "$net" dev eth0 || :
    echo "${net%/*} $FQDN" >> /etc/hosts || :
done

# If we have an http_proxy set, then we should make sure
# we have no-proxy setup correctly and use it.
if [[ $http_proxy ]] ; then
    # Now that we have shiny new IP addresses, make sure that Squid has the right
    # addresses in place for always_direct exceptions, and pick up the new proxy
    # environment variables.
    (
        . bootstrap.sh
        chef-solo -c /opt/opencrowbar/core/bootstrap/chef-solo.rb -o "${proxy_recipes}"
    )
    . /etc/profile

    # Make sure that Crowbar is running with the proper environment variables
    service crowbar stop
    service crowbar start
    while ! /opt/opencrowbar/core/bin/crowbar -U crowbar -P crowbar users list; do
        sleep 1
    done
fi

crowbar nodes commit "$FQDN"

# Mark the node as alive.
crowbar nodes update "$FQDN" '{"alive": true}'
echo "Configuration Complete, you can watch annealing from the UI.  \`su - crowbar\` to begin managing the system."
# Converge the admin node.
crowbar converge && date

