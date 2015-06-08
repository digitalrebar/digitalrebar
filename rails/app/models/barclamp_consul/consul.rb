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

  def on_deployment_create(dr)
    DeploymentRole.transaction do
      if Attrib.get("consul-encrypt",dr) == "change_me"
        Attrib.set("consul-encrypt",dr,SecureRandom.base64)
      end
      if Attrib.get("consul-acl-master-token",dr) == "change_me"
        Attrib.set("consul-acl-master-token",dr,SecureRandom.uuid)
      end
    end
  end

  def on_node_bind(nr)
    NodeRole.transaction do
      # If this is our first Consul node, have it operate in server mode.
      # Otherwise, it is a client.
      consuls = NodeRole.where(deployment_id: nr.deployment_id, role_id: nr.role_id).count
      Attrib.set("consul-mode",nr,consuls == 1 ? "server" : "client")
    end
  end

  def on_todo(nr)
    Attrib.transaction do
      if Attrib.get("consul-address",nr).nil?
        Attrib.set("consul-address",nr,nr.node.addresses.first.addr)
      end
    end
    return if Attrib.get('consul-mode',nr) == "client"
    DeploymentRole.transaction do
      consuls = NodeRole.where(deployment_id: nr.deployment_id,
                               role_id: nr.role_id).reject do |cnr|
        Attrib.get('consul-mode',cnr) == "client"
      end

      servers = consuls.map{|cnr| "[#{cnr.node.addresses.first.addr}]:8301"}
      to_join = consuls.reject{|cnr|cnr.id == nr.id}.map{|cnr| "[#{cnr.node.addresses.first.addr}]:8301"}
      Rails.logger.info("Updating global Consul servers: #{servers.inspect}")
      Rails.logger.info("Updating this Consul's servers: #{to_join.inspect}")
      Attrib.set("consul-servers",nr.deployment_role,servers)
      Attrib.set("consul-bootstrap-expect",nr.deployment_role,consuls.length)
      Attrib.set("consul-servers",nr,to_join)
    end
  end
end
