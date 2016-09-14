# Copyright 2015, RackN
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

require 'json'

class ::BarclampNetwork::Attrib::AllConfig < Attrib

  def poke(n)
    true
  end
  
  def get(data, source=:all, committed=false)
    Rails.logger.info("#{self.class.name}: Resolving all network data for #{data.name}")
    Attrib.transaction do
      from = __resolve(data)
      Rails.logger.info("#{self.class.name}: 1")
      return case
             when from.is_a?(Node)
               res = {}
               from.node_roles.order("cohort ASC").each do |nr|
                 next unless nr.role.is_a?(BarclampNetwork::Role)
                 Rails.logger.info("#{self.class.name}: 2: #{nr.role.name}")          
                 res.deep_merge!(nr.all_committed_data)
               end
               Rails.logger.info(JSON.pretty_generate(res))
               super(res)
             else
               super
             end
    end
  end
end
