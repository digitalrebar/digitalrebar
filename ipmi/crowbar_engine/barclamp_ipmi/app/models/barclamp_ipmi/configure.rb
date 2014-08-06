# Copyright 2014 Victor Lowther
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

class BarclampIpmi::Configure < Role

  def on_active(nr)
    username = Attrib.get('ipmi-username',nr.node)
    authenticator = Attrib.get('ipmi-password',nr.node)
    endpoint = Network.address(node: nr.node, network: "bmc", range: "host").address.addr
    unless nr.node.hammers.find_by(type: "BarclampIpmi::IpmiHammer")
      # We must have a configured IPMI controller to operate.
      Hammer.bind(manager_name: "ipmi",
                       username: username,
                       authenticator: authenticator,
                       endpoint: endpoint,
                       node: nr.node)
    end
    unless nr.node.hammers.find_by(type: "BarclampIpmi::WsmanHammer")
      endpoint = BarclampIpmi::WsmanHammer.probe(nr.node)
      return unless endpoint
      Hammer.bind(manager_name: "wsman",
                       username: username,
                       authenticator: authenticator,
                       endpoint: endpoint,
                       node: nr.node)
    end
  end
end
