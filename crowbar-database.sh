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
. ./bootstrap.sh

which consul && consul watch -service=crowbar-database -type=service  | grep -q Node && exit 0

check_hostname

# install the database
chef-solo -c /opt/opencrowbar/core/bootstrap/chef-solo.rb -o "${database_recipes}"

