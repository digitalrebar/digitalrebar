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

class Publisher
  @@channel = nil
  @@connection = nil
  @@success = false

  def self.get_channel
    return @@channel if @@channel

    unless @@connection
      # XXX: Use diplomat here
      @@connection = Bunny.new
      @@connection.start
    end
    @@channel = @@connection.create_channel
  end

  def self.close_channel
    @@channel.close if @@channel
    @@connection.close if @@connection
    @@connection = nil
    @@channel = nil
  end

  # In order to publish message we need a exchange name.
  # Note that RabbitMQ does not care about the payload -
  # we will be using JSON-encoded strings
  def self.publish_event(who, type, message = {})
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

