#!/usr/bin/env ruby
# encoding: utf-8
# Copyright 2012, Dell
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
#

require 'bunny'
require 'json'
require 'diplomat'
require '/opt/opencrowbar/core/rails/lib/consul_access'
require '/opt/opencrowbar/core/rails/lib/ip'

if ARGV.empty?
  abort "Usage: #{$0} [binding key]"
end

# Lookup amqp service and build url for bunny
s = ConsulAccess.getService('amqp-service')
if s == nil or s.ServiceAddress == nil
  puts "AMQP Service not available, retry later"
  exit 1
end

addr = IP.coerce(s.ServiceAddress)
hash = {}
hash[:user] = 'crowbar'
hash[:pass] = 'crowbar'
if addr.v6?
  hash[:host] = "[#{addr.addr}]"
else
  hash[:host] = addr.addr
end
hash[:vhost] = '/opencrowbar'
hash[:port] = s.ServicePort.to_i

conn = Bunny.new(hash)
conn.start

ch  = conn.create_channel
x   = ch.topic("opencrowbar")
q   = ch.queue("")

ARGV.each do |severity|
  q.bind(x, :routing_key => severity)
end

puts " [*] Waiting for logs. To exit press CTRL+C"

begin
  q.subscribe(:block => true) do |delivery_info, properties, body|
    body = JSON.parse(body)
    if delivery_info.routing_key =~ /^node_role./
      puts " [x] #{delivery_info.routing_key}:#{body["id"]}"
    elsif delivery_info.routing_key =~ /^node./
      puts " [x] #{delivery_info.routing_key}:#{body["name"]}"
    elsif delivery_info.routing_key =~ /^role./
      puts " [x] #{delivery_info.routing_key}:#{body["id"]}"
    else
      puts " [x] #{delivery_info.routing_key}:#{body}"
    end
  end
rescue Interrupt => _
  ch.close
  conn.close
end

