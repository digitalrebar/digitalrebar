# Copyright 2013, Dell
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

class BarclampIpmi::BmcNatRouter < Role


  def on_node_change(node)
    Rails.logger.info("ipmi-bmc-nat-router: Updating for changed node #{node.name}")
    rerun_my_noderoles node
  end

  def rerun_my_noderoles node
    NodeRole.transaction do
      node_roles.committed.each do |nr|
        bmc_net = BarclampNetwork::Network.where(:name => "bmc").first
        bmc_subnet = bmc_net.allocations.first.address.network.addr
        admin_subnet = node.addresses.reject{|a|a.v6?}.sort.first.network.addr
        Run.enqueue(nr) unless bmc_subnet == admin_subnet
      end
    end
  end

end