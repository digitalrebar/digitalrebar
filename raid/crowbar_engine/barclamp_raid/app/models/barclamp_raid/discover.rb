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

  def on_active(nr)
    # Do nothing unless we discovered RAID controllers we can manage on this node.
    controllers = (nr.wall["crowbar_wall"]["raid"]["controllers"] || [] rescue [])
    if controllers.empty?
      Rails.logger.info("No RAID controllers on #{nr.node.name}")
      return
    end

    # Since we have detected controllers we can manage, go ahead and bind the
    # raid-configure role to this node.
    rcr_role = Role.find_by!(name: 'raid-configure')
    unless nr.node.node_roles.find_by(role_id: rcr_role.id)
      Rails.logger.info("Adding raid-configure role to #{nr.node.name}")
      raid_config = rcr_role.add_to_node(nr.node)
      raid_config.commit!
      # Make the raid-configure role depend on the crowbar-hardware-configured noderole.
      Rails.logger.info("Making crowbar-hardware-configured on #{nr.node.name} depend on raid-configure role.")
      raid_config.add_child('crowbar-hardware-configured')
    end
  end
end
