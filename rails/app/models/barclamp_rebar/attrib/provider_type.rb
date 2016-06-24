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
      d = case
          when from.is_a?(Node)
            from.provider.class
          when from.is_a?(NodeRole)
            from.node.provider.class
          else
            ""
      end
      return d.downcase.subprovider("provider","")

  end

end
