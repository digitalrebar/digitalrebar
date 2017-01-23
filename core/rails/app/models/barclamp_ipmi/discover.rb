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

    # If the detected IPMI controller needs any quirks, make sure they are added
    # to the node's quirklist
    quirklist = []
    score = 0
    quirkset = "default"
    Attrib.get("ipmi-quirklist",nr).each do |k,v|
      quirk_score = StructureMatch.new(v["match"]).score(nr.wall["rebar_wall"]["ipmi"])
      next unless quirk_score > score
      score = quirk_score
      quirkset = k
      quirklist = v["quirklist"]
    end
    Rails.logger.info("IPMI: #{nr.node.name} using #{quirkset} quirks.")
    nr.node.merge_quirks(quirklist)

    icr_role = Role.find_by!(name: 'ipmi-configure')
    chc_role = Role.find_by!(name: 'rebar-hardware-configured')
    ipmi_config = nr.node.node_roles.find_by(role_id: icr_role.id)
    unless ipmi_config
      Rails.logger.info("Adding ipmi-configure role to #{nr.node.name}")
      # Force the config role into the same deployment as the discover role
      ipmi_config = icr_role.add_to_node(nr.node)
    end
    chc_role.add_to_node(nr.node)

    # If we do not have a BMC network defined, we cannot continue.
    # lookup up a BMC network-based upon the category/group of the admin network.
    netgroup = nil
    NetworkAllocation.node(nr.node).each do |na|
      if na.network.category == 'admin'
        netgroup = na.network.group
        break
      end
    end

    bmcnet = netgroup ? Network.find_by(category: "bmc", group: netgroup) : nil
    if !bmcnet
      if Attrib.get('ipmi-configure-networking',ipmi_config)
        Attrib.set('ipmi-configure-networking',ipmi_config, false)
      end
    else
      unless Attrib.get('ipmi-configure-networking',ipmi_config) 
        Attrib.set('ipmi-configure-networking',ipmi_config, true)
      end
      if bmcnet.conduit == "bmc"
        # All BMCs should get addresses from the host range.
        bmcrange = NetworkRange.find_by!(network_id: bmcnet.id, name: "host")
        # If we have already allocated an IP address for this BMC, we are done.
        unless NetworkAllocation.find_by(node_id: nr.node.id, network_range_id: bmcrange.id)
          Rails.logger.info("No address allocated for the BMC on #{nr.node.name}")
          addr = nr.wall["rebar_wall"]["ipmi"]["laninfo"]["ipaddr"]
          mask = nr.wall["rebar_wall"]["ipmi"]["laninfo"]["netmask"]
          suggested_addr = IP.coerce("#{addr}/#{mask}")
          address = bmcrange.allocate(nr.node,suggested_addr)
          # Once we have an address allocated, we will have a noderole created
          # for this node for the network-bmc role.
          # Commit it.
          Rails.logger.info("BMC address #{address.address.to_s} allocated for #{nr.node.name}")
          nr.node.node_roles.find_by!(role_id: bmcnet.role.id).commit!
        end
      elsif bmcnet.conduit == "dhcp"
        unless Attrib.get('ipmi-use-dhcp',ipmi_config)
          Attrib.set('ipmi-use-dhcp',ipmi_config, true)
        end
      end
    end
  end
end
