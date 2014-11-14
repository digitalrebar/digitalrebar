#
# Copyright 2011, Dell
# Copyright 2014, Victor Lowther
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
name             "ipmi"
maintainer       "Opencrowbar Team"
maintainer_email "opencrowbar@googlegroups.com"
license          "Apache 2.0"
description      "Configures IPMI/BMC parameters for management access"
long_description  "Configures IPMI/BMC parameters for management access"
version          "0.1.3"
recipe "ipmi::master", "Configures a node to act as controller for BMCs in the cluster"
recipe "ipmi::discover", "Discovers basic information about IPMI on a node"
recipe "ipmi::configure", "Configures the BMC on a node to allow an ipmi::master to manage itout of band"
recipe "ipmi::ipmitool", "Installs IPMItool on a node."
