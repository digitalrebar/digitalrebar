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
date

# setup & load env info
if [[ ! -f /etc/profile.d/crowbar.sh ]]; then
    cat > /etc/profile.d/crowbar.sh <<EOF
if ! fgrep -q '/opt/opencrowbar/core/bin' < <(echo \$PATH); then
    export PATH=\$PATH:/opt/opencrowbar/core/bin
fi
EOF
fi
. ./bootstrap.sh

if [[ ! $RAILS_ENV ]]; then
	export RAILS_ENV=$1
fi

. /etc/profile
./setup/00-crowbar-rake-tasks.install && \
    ./setup/01-crowbar-start.install && \
    ./setup/02-make-machine-key.install || {
    echo "Failed to bootstrap the Crowbar UI"
    exit 1
}

