# Copyright 2014, Dell
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

class BarclampNetwork::Server < Role


  def conduit?
    false
  end

  # used by the network-server role to get interfaces
  def interfaces
    o = {}
    if name.eql? 'network-server'
      # use the first one of these -> it should be the system deployment
      raw = Attrib.get 'network_interface_maps', Deployment.system
      raw.each { |im| o[im["pattern"]] = im["bus_order"] }
    else
      raise "this model only applies to the network-server named role"
    end
    o
  end

  def update_interface(pattern, bus_order)
    # use the first one of these -> it should be the system deployment
    bus_order = bus_order.split("|") unless bus_order.is_a? Array
    map = Attrib.get 'network_interface_maps', Deployment.system
    new_map = map | [{ "pattern"=>pattern, "bus_order"=>bus_order }]
    DeploymentRole.transaction do
      Attrib.set 'network_interface_maps', Deployment.system, new_map, :user
    end
  end

end
