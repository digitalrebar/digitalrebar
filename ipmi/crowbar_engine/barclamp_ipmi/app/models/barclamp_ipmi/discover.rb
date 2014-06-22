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

class BarclampIpmi::Discover < Role

  def on_active(nr)
    # Do nothing unless we discovered that we can use a BMC on this node.
    return unless nr.wall["ipmi"]["bmc_enable"]
    # If we do not have a BMC network defined, we cannot continue.
    bmcnet = Network.find_by(name: "bmc")
    return unless bmcnet
    # All BMCs should get addresses from the host range.
    bmcrange = NetworkRange.find_by!(network_id: bmcnet.id, name: "host")
    # If we have already allocated an IP address for this BMC, we are done.
    return if NetworkAllocation.find_by(node_id: nr.node.id, network_range_id: bmcrange.id)
    suggested_addr = IP.coerce("#{nr.wall["crowbar_wall"]["ipmi"]["address"]}/#{nr.wall["crowbar_wall"]["ipmi"]["netmask"]}")
    address = bmcrange.allocate(nr.node,suggested_addr)
    # Once we have an address allocated, we will have a noderole created
    # for this node for the network-bmc role.
    # Commit it.
    nr.node.node_roles.find_by!(role_id: bmcnet.role.id).commit!
    # Now that we have the appropriate network information, we need to bind
    # the ipmi-configure role to this node.
    ipmi_config = Role.find_by!(name: 'ipmi-configure').add_to_node(nr.node)
    ipmi_config.commit!
    # Keep the noderole graph sane by making the managed_node role for this node
    # depend on our newly-added ipmi config noderole.
    managed_node = nr.node.node_roles.find_by!(role_id: Role.find_by!(name: 'crowbar-managed-node'))
    managed_node.add_parent(ipmi_config)
  end
end
