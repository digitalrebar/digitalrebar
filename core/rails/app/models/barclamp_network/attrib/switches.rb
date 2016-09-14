# Copyright 2013, Dell
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

class BarclampNetwork::Attrib::Switches < Attrib

  # switches has to calculate info from the data, not just look it up
  def data(data,from=:all, committed=false)

    raw = super
    switches = {}
    if raw and raw.length > 0
      begin 
        raw.map do |k, v|
          switches[k] = {
            'name'=>unknower(v['switch_name']),
            'port'=>unknower(v['switch_port']),
            'unit'=>unknower(v['switch_unit']),
            'raw'=>v['switch_port_name'],
            'link'=>v['port_link']
          }
        end
      rescue
        # do nothing
      end
    end
    # we want something safe, just in case!
    switches['eth0'] = {"name"=>-1, "port"=>-1, "unit"=>-1, "link"=>false} if switches.length==0

    return switches
  end

  private 

  def unknower(v)
    (v == -1 ? I18n.t('not_set') : v)
  end

end
