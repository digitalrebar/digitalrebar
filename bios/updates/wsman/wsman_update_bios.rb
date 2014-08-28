#!/usr/bin/ruby
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

require 'wsman'
require 'wsman_update'

opts = { :prov_ip => (ARGV[3] || "192.168.124.10"), :prov_port => (ARGV[4] || "8091").to_i,
         :host => ARGV[0], :port => 443, 
         :user => ARGV[1], :password => ARGV[2], 
         :debug_time => false }
test_update(opts)

