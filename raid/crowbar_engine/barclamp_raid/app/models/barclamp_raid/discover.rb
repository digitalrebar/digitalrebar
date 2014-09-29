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

class BarclampRaid::Discover < Role

  def on_proposed(nr)
    nr.add_child('crowbar-managed-node')
  end

  def on_active(nr)
    # Do nothing unless we discovered RAID controllers we can manage on this node.
    controllers = (nr.wall["crowbar_wall"]["raid"]["controllers"] || [] rescue [])
    if controllers.empty?
      Rails.logger.info("No RAID controllers on #{nr.node.name}")
      return
    end

    # Since we have detected controllers we can manage, go ahead and bind the
    # raid-configure and crowbar-hardware-configured roles to this node.
    rcr_role = Role.find_by!(name: 'raid-configure')
    chc_role = Role.find_by!(name: 'crowbar-hardware-configured')
    NodeRole.transaction do
      rcr_noderole = rcr_role.add_to_node(nr.node)
      chc_noderole = chc_role.add_to_node(nr.node)
      rcr_noderole.add_child(chc_noderole)
    end
  end
end
