# Copyright 2015, Greg Althaus
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
#

require 'monitor'

class Publisher
  @@channel = nil
  @@connection = nil
  @@success = false
  @@lock = Monitor.new

  def self.get_channel
    @@lock.synchronize do
      return @@channel if @@channel

      unless @@connection
        # Lookup amqp service and build url for bunny
        s = ConsulAccess.getService('amqp-service')
        if s == nil || s.ServiceAddress == nil
          raise "AMQP Service not available"
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

        Rails.logger.debug("Attempting to connection to AMQP service: #{hash}")
        @@connection = Bunny.new(hash)
        @@connection.start
      end
      @@channel = @@connection.create_channel
    end
    @@channel
  end

  def self.close_channel
    @@lock.synchronize do
      @@channel.close if @@channel
      @@connection.close if @@connection
      @@connection = nil
      @@channel = nil
    end
  end

  # In order to publish message we need a exchange name.
  # Note that RabbitMQ does not care about the payload -
  # we will be using JSON-encoded strings
  def self.publish_event(who, type, message = {})
    @@lock.synchronize do
      begin
        channel = self.get_channel
        x = channel.topic("opencrowbar")
        # and simply publish message
        x.publish(message.to_json, :routing_key => "#{who}.#{type}")
        @@success = true
      rescue Exception => e
        Rails.logger.fatal("publish_event failed: #{e.message}") if @@success
        self.close_channel
      end
    end
  end

end
