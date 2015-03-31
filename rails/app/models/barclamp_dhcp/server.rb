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

class BarclampDhcp::Server < Role

  def sysdata(nr)
    networks = {}
    Network.find_all_by_category("admin").each do |net|
      networks[net.name] = net.to_template
    end

    {
        "crowbar" => {
            "dhcp" => { "networks" => networks }
        }
    }
  end

end
