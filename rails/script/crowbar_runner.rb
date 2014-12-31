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


class Consumer
  def initialize
    @queue = []
  end

  def queue_message(metadata, payload)
puts "GREG: Queuing a message"
    @queue << { :metadata => metadata, :payload => payload }
  end

  def run_queue(channel)
puts "GREG: in run_queue"
    new_queue = @queue
    @queue = []

puts "GREG: queue depth = #{new_queue.size}"
    return if new_queue.empty?

    new_queue.each do |d|
      metadata = d[:metadata]
      payload = d[:payload]

      action_response = Time.now.to_s
      puts "GREG: Payload = #{payload}"
      if payload == "get.time"
        # Nothing
      elsif payload =~ /^chef-client/
        result = %x{#{payload}}
        action_response += ":#{$?}:#{result}"
      end

      puts "[requests] Got a request #{metadata.message_id}. Sending a reply..."
      channel.default_exchange.publish(action_response,
                                       :routing_key => metadata.reply_to,
                                       :correlation_id => metadata.message_id,
                                       :immediate => true,
                                       :mandatory => true)

    end
puts "GREG: Sleeping"
#    sleep(10)
puts "GREG: Done"
  end
end

EventMachine.run do
  connection = AMQP.connect(:host => "192.168.124.10")
  channel = AMQP::Channel.new(connection)

  consumer = Consumer.new
  queue_name = "#{`hostname`.strip}.runner"
  requests_queue = channel.queue(queue_name, :exclusive => true, :auto_delete => true)

  requests_queue.subscribe(&consumer.method(:queue_message))

  EventMachine.add_periodic_timer(1.0) do
    consumer.run_queue(channel)
  end

  Signal.trap("INT") { connection.close { EventMachine.stop } }

end


