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
        "rebar" => {
            "dhcp" => { "networks" => networks }
        }
    }
  end

  # Event triggers for node creation.
  # roles should override if they want to handle network addition
  def on_network_create(network)
    rerun_my_noderoles
  end

  # Event triggers for network destruction.
  # roles should override if they want to handle network destruction
  def on_network_delete(network)
    rerun_my_noderoles
  end

  # Event hook that will be called every time a network is saved if any attributes changed.
  # Roles that are interested in watching networks to see what has changed should
  # implement this hook.
  #
  # This does not include IP allocation/deallocation.
  def on_network_change(network)
    rerun_my_noderoles
  end

  private

  def rerun_my_noderoles
    to_enqueue = []
    node_roles.each do |nr|
      nr.with_lock('FOR NO KEY UPDATE') do
        to_enqueue << nr
      end
    end
    to_enqueue.each {|nr| Run.enqueue(nr)}
  end

end
