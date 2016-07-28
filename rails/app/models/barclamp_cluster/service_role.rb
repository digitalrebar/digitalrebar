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

class BarclampCluster::ServiceRole < Role

  def sync_on_destroy(nr, *args)
    collect_addresses(nr, args)
  end

  def on_todo(nr, *args)
    collect_addresses(nr, args)
  end

  def on_active(nr, *args)
    collect_addresses(nr, args)
  end

  private

  def collect_addresses(nr, *args)

    # allow users to set if we only collect addresses from active node-roles (default false)
    active_only = Attrib.get("#{nr.role.name}-active-only",nr.deployment_role) rescue false
    Rails.logger.debug("Service Role for #{nr.role.name}, active only is #{active_only}")

    aname = "#{nr.role.name}-addresses"
    hname = "#{nr.role.name}-hostnames"
    ipname = "#{nr.role.name}-address"
    netname = "#{nr.role.name}-network"
    address = nil
    addresses = []
    hosts = []

    # when we deal w/ node_roles, we want transactions for protection
    DeploymentRole.transaction do

      # look up network type from attrib maps
      # use node-control-address or private-node-control-address
      # for internal networking, you can use v4/network_name or v6/network_name
      map = Attrib.get(netname,nr) || "node-control-address"
      Rails.logger.debug("For #{nr.role.name}, using network #{map}")

      d = nr.deployment
      # find all the similar node_roles in the deployment
      d.node_roles.where(role_id: nr.role_id).each do |nrs|

        n = nrs.node
        str_addr = nil
        case map.split("/")[0]
        when "v4"
          str_addr ||= n.address(:v4_only, [map.split("/")[1]]).addr
        when "v6"
          str_addr ||= n.address(:v6_only, [map.split("/")[1]]).addr
        else
          str_addr = n.address(:v4_only, [map]).addr rescue n.address.addr
        end

        # set address for this node 
        if n.id == nr.node.id
          address = str_addr
        end

        Rails.logger.debug("For #{nr.role.name}, adding node #{n.name} with address #{address}")

        # with active_only, we only include active roles
        unless active_only and !nrs.active?
          # collect cluster addresses
          addresses << str_addr if str_addr
          hosts << n.name
        end

      end

      # set the address on the deployment role (THIS IS SHARED, NOT ON THE NODE_ROLE)
      Rails.logger.info("Updating #{nr.role.name} #{aname}: #{addresses.inspect}")
      Attrib.set(aname,nr.deployment_role,addresses) rescue Rails.logger.warn("#{nr.role.name} #{aname} attrib not defined")

      # set the names on the deployment role (THIS IS SHARED, NOT ON THE NODE_ROLE)
      Rails.logger.info("Updating #{nr.role.name} #{hname}: #{hosts.inspect}")
      Attrib.set(hname,nr.deployment_role,hosts) rescue Rails.logger.info("#{nr.role.name} #{hname} attrib not defined (may be ok)")

      # set the address for the node
      Rails.logger.info("Updating #{nr.role.name} #{ipname}: #{address.inspect}")
      Attrib.set(ipname,nr,address) rescue Rails.logger.warn("#{nr.role.name} #{ipname} attrib not defined")

    end

    true

  end

end
