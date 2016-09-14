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

  def do_transition(nr, data)
    update_log(nr, "Detecting Raid Controllers")
    controllers = nr.node.actions[:raid].detect(nr).map{|c|c.to_hash} || []
    Attrib.set('raid-detected-controllers',nr,controllers,:wall)

    # Do nothing unless we discovered RAID controllers we can manage on this node.
    if controllers.empty?
      update_log(nr, "No RAID controllers found")
      Rails.logger.info("No RAID controllers on #{nr.node.name}")
      return true
    end

    update_log(nr, "Found controllers: #{controllers}\nAdd configuration roles")

    # Since we have detected controllers we can manage, go ahead and bind the
    # raid-configure and rebar-hardware-configured roles to this node.
    rpc_role = Role.find_by!(name: 'raid-post-configure')
    chc_role = Role.find_by!(name: 'rebar-hardware-configured')

    # Force these nodes into the same deployment
    rpc_noderole = rpc_role.add_to_node(nr.node)
    chc_noderole = chc_role.add_to_node(nr.node)
  end

  def update_log(nr, string)
    nr.runlog += string + "\n"
    nr.save
  end

end
