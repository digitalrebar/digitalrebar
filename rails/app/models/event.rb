# Copyright 2016, RackN
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

require 'securerandom'
class Event < ActiveRecord::Base

  def self.fire(obj, params)
    evt = Event.create!(params: params,
                        target_class: obj.class.name,
                        target: obj.to_json)
    evt.run(obj)
  end

  def run(obj)
    Rails.logger.info("Event: event #{id} fired with params #{params.to_json}")
    wherefrags = []
    params.each do |k,v|
      wherefrags << "(event_selectors.selector @> '#{ {k.to_s => v}.to_json}'::jsonb)"
    end
    res = []
    # This needs to handle wildcard subscriptions and more complex
    # logic at some point in the future, but for now, straight
    # equality matching will do.
    matched_selectors = EventSelector.where(wherefrags.join(" AND ")).order(:id).distinct
    self.event_selectors = matched_selectors.all.map{|es| es.uuid}
    self.save!
    matched_selectors.each do |ms|
      es = ms.event_sink
      Rails.logger.info("Event: #{params} matched #{ms.selector}")
      Rails.logger.info("Event: calling #{es.endpoint} for #{ms.selector} with #{obj.inspect}")
      res << es.run(self,obj, ms.selector)
    end
    return res
  end
end
