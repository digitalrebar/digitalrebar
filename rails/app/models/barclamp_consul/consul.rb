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
      # If this is our first Consul node, have it operate in bootstrap mode.
      # Otherwise, it is a client.
      Attrib.set("consul-mode",nr,nr.role.node_roles.count == 1 ? "bootstrap" : "client")
    end
  end

  def on_todo(nr)
    if Attrib.get("consul-address",nr).nil?
      Attrib.set("consul-address",nr,nr.node.addresses.first.addr)
    end
    return if Attrib.get("consul-mode",nr) == "bootstrap"
    Resolv::DNS.open(nameserver_port: [['127.0.0.1',8600]],
                     nameserver: '127.0.0.1',
                     search: "consul",
                     ndots: 1) do |resolv|
      Attrib.set("consul-servers",nr,
                 resolv.getaddresses('consul.service.consul').map{|a|"[#{a.to_s}]"})
    end
  end

end
