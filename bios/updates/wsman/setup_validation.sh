#!/bin/bash
# Copyright (c) 2013 Dell Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

# Run from admin node

cd /root
cp -r /updates/wsman .
cp /opt/dell/barclamps/bios/chef/cookbooks/bios/libraries/* /root/wsman

rpm -Uvh /opt/dell/barclamps/bios/wsman/libwsman1-2.2.7-1.x86_64.rpm
rpm -Uvh /opt/dell/barclamps/bios/wsman/wsmancli-2.2.7.1-11.x86_64.rpm 

