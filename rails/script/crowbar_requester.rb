#!/usr/bin/env ruby
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

require "rubygems"
gem "amqp"
require "amqp"

hostname=ARGV[0]

EventMachine.run do
  connection = AMQP.connect(:host => "192.168.124.10")
  channel = AMQP::Channel.new(connection)

  replies_queue = channel.queue("", :exclusive => true, :auto_delete => true)
  replies_queue.subscribe do |metadata, payload|
    puts "[response] Response for #{metadata.correlation_id}: #{payload.inspect}"
  end

  # request time from a peer every 3 seconds
  EventMachine.add_periodic_timer(3.0) do
    puts "[request] Sending a request..."
    channel.default_exchange.publish("chef-client -S http://192.168.124.10:4000",
                                     :routing_key => "#{hostname}.runner",
                                     :message_id => Kernel.rand(10101010).to_s,
                                     :reply_to => replies_queue.name,
                                     :immediate => true)
  end


  Signal.trap("INT") { connection.close { EventMachine.stop } }
end

