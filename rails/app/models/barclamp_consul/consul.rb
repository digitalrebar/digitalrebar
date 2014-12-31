# Copyright 2014, Victor Lowther
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

require 'resolv'

class BarclampConsul::Consul < Role

  def on_proposed(nr)
    NodeRole.transaction do
      sd = { "consul" => {}}
      # If this is our first Consul node, have it operate in bootstrap mode.
      # Otherwise, it is a client.
      sd["consul"]["service_mode"] = nr.role.node_roles.count == 1 ? "bootstrap" : "client"
      # We will have Consul talk over IPv6
      sd["consul"]["bind_addr"] = Network.address(network: "admin", range: "host-v6", node: nr.node.id).address.addr
      nr.sysdata = sd
    end
  end

  def on_todo(nr)
    return if nr.sysdata["consul"]["service_mode"] == "bootstrap"
    sd = nr.sysdata
    Resolv::DNS.open(nameserver_port: [['127.0.0.1',8600]],
                     nameserver: '127.0.0.1',
                     search: "consul",
                     ndots: 1) do |resolv|
      servers = resolv.getaddresses('consul.service.consul').map{|a|"[#{a.to_s}]"}
      NodeRole.transaction do
        sd["consul"]["servers"] = servers
        nr.sysdata = sd
      end
    end
  end

end
