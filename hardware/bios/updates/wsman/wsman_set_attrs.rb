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
require 'wsman_attributes'

opts = { :host => ARGV[0], :user => ARGV[1], :password => ARGV[2], :port => 443, :debug_time => false }
attrs = {}

count=ARGV.length - 3
count.times do |i|
  parts = ARGV[i + 3].split("=")
  attrs[parts[0]] = parts[1]
  puts "GREG: setting #{parts[0]} to #{parts[1]}"
end

answer, message, node = test_set_attributes(attrs, opts)

puts "answer = #{answer}"
puts "message = #{message}"

