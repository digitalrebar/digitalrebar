#
# Cookbook Name: bios
# Role: bios-install
#
# Copyright (c) 2011 Dell Inc.
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

name "bios"
description "Dell BIOS role - Install bios update and configure Dell BIOS parameters as defined for the machine's role"
run_list(
  "recipe[bios::bios-install]",
  "recipe[bios::bios-configure]"
)

