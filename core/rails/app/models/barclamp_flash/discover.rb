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

class BarclampFlash::Discover < Role

  def do_transition(nr, data)
    unless Attrib.get("enable-flash-subsystem",nr.node)
        update_runlog("Flash subsystem is not enabled. Skipping this function.")
        update_runlog("Set enable-flash-subsystem to true and rerun role to start Flash processing.")
        return
    end

    update_log(nr, "Determining Flash System to use:")
    mfgr = Attrib.get("baseboard_manufacturer",nr.node)
    rpc_role = if mfgr == 'Dell Inc.'
      Role.find_by!(name: 'dell-firmware-flash')
    else
      Role.find_by!(name: 'firmware-flash')
    end
    chc_role = Role.find_by!(name: 'rebar-hardware-configured')
    rpc_role.add_to_node(nr.node)
    chc_role.add_to_node(nr.node)
    update_log(nr, "Added dell-firmware-flash role to node")
  end

  def update_log(nr, string)
    nr.runlog += string + "\n"
    nr.save
  end

end
