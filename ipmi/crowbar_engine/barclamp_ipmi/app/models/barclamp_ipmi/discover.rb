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

require 'structurematch'
class BarclampIpmi::Discover < Role

  def on_active(nr)
    # Do nothing unless we discovered that we can use a BMC on this node.
    unless nr.wall["ipmi"]["bmc_enable"]
      Rails.logger.info("BMC not enabled on #{nr.node.name}")
      return
    end

    # If we do not have a BMC network defined, we cannot continue.
    # lookup up a BMC network-based upon the category/group of the admin network.
    group_name = 'default'
    NetworkAllocation.node(nr.node).each do |na|
      if na.network.category == 'admin'
        group_name = na.network.group
        break
      end
    end
    bmcnet = Network.find_by(category: "bmc", group: group_name)
    unless bmcnet
      Rails.logger.info("No BMC network created for #{group_name}.")
      return
    end

    # If the detected IPMI controller needs any quirks, make sure they are added
    # to the node's quirklist
    quirklist = []
    score = 0
    quirkset = "default"
    Attrib.get("ipmi-quirklist",nr).each do |k,v|
      quirk_score = StructureMatch.new(v["match"]).score(nr.wall["crowbar_wall"]["ipmi"])
      next unless quirk_score > score
      score = quirk_score
      quirkset = k
      quirklist = v["quirklist"]
    end
    Rails.logger.info("IPMI: #{nr.node.name} using #{quirkset} quirks.")
    nr.node.merge_quirks(quirklist)

    # All BMCs should get addresses from the host range.
    bmcrange = NetworkRange.find_by!(network_id: bmcnet.id, name: "host")
    # If we have already allocated an IP address for this BMC, we are done.
    unless NetworkAllocation.find_by(node_id: nr.node.id, network_range_id: bmcrange.id)
      Rails.logger.info("No address allocated for the BMC on #{nr.node.name}")
      addr = nr.wall["crowbar_wall"]["ipmi"]["laninfo"]["ipaddr"]
      mask = nr.wall["crowbar_wall"]["ipmi"]["laninfo"]["netmask"]
      suggested_addr = IP.coerce("#{addr}/#{mask}")
      address = bmcrange.allocate(nr.node,suggested_addr)
      # Once we have an address allocated, we will have a noderole created
      # for this node for the network-bmc role.
      # Commit it.
      Rails.logger.info("BMC address #{address.address.to_s} allocated for #{nr.node.name}")
      nr.node.node_roles.find_by!(role_id: bmcnet.role.id).commit!
    end
    # Now that we have the appropriate network information, we need to bind
    # the ipmi-configure role to this node.
    icr_role = Role.find_by!(name: 'ipmi-configure')
    unless nr.node.node_roles.find_by(role_id: icr_role.id)
      Rails.logger.info("Adding ipmi-configure role to #{nr.node.name}")
      ipmi_config = icr_role.add_to_node(nr.node)
      ipmi_config.commit!
      # Keep the noderole graph sane by making the managed_node role for this node
      # depend on our newly-added ipmi config noderole.
      Rails.logger.info("Making crowbar-managed-node on #{nr.node.name} depend on ipmi-configure role.")
      ipmi_config.add_child('crowbar-managed-node')
    end
  end
end
