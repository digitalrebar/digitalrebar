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

class BarclampRebar::ServiceRole < Role

  def sync_on_destroy(nr, *args)
    collect_addresses(nr, args)
  end

  def on_active(nr, *args)
    collect_addresses(nr, args)
  end

  private

  def collect_addresses(nr, *args)

    aname = "#{nr.role.name}-addresses"
    netname = "#{nr.role.name}-network"
    addresses = []

    # when we deal w/ node_roles, we want transactions for protection
    DeploymentRole.transaction do

      # look up network type from attrib maps
      # use node-control-address or private-node-control-address
      # for internal networking, you can use v4/network_name or v6/network_name
      map = Attrib.get(netname,nr) || "node-control-address"

      d = nr.deployment
      # find all the similar node_roles in the deployment
      d.node_roles.where(role_id: nr.role_id).each do |nrs|
        # we only want the active ones, all others are suspect
        if nrs.active?
          n = nrs.node
          case map.split("/")[0]
          when "v4"
            str_addr ||= n.address(:v4_only, [map.split("/")[1]]).to_s
          when "v6"
            str_addr ||= n.address(:v6_only, [map.split("/")[1]]).to_s
          else
            str_addr = n.get_attrib(map) rescue n.address.to_s
          end
          addresses << str_addr if str_addr
        end
      end

      # set the address on the deployment role (THIS IS SHARED, NOT ON THE NODE_ROLE)
      Rails.logger.info("Updating #{nr.role.name} #{aname}: #{addresses.inspect}")
      Attrib.set(aname,nr.deployment_role,addresses)

    end

    true

  end

end
