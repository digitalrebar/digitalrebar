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

class BarclampRebar::Attrib::ProviderType < Attrib

  # Lookup the node's provider type
  def get(data,source=:all, committed=false)

      from = __resolve(data)
      p = case
          when from.is_a?(Node)
            from.provider
          when from.is_a?(NodeRole)
            from.node.provider
          else
            nil
      end
      # no data, return
      return {} unless p
      # make hash with info
      o = { type: p.class.downcase.sub("provider",""), name: p.name }
      o[:auth_details] = p.auth_details
      return o

  end

end