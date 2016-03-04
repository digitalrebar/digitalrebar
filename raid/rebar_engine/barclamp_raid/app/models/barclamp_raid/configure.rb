# Copyright 2016 Greg Althaus
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

class BarclampRaid::Configure < Role

  def do_transition(nr, data)
    enabled = Attrib.get('raid-enable',nr)
    unless enabled
      update_log(nr, "raid-configure not enabled, skipping raid config")
      return true
    end
    update_log(nr, "Configuring Raid Controllers")
    config = nr.node.actions[:raid].converge(nr).map{|c|c.to_hash}
    update_log(nr, "Configured Raid Controllers: #{config}")
    Attrib.set('raid-configured-volumes',nr,config,:wall)
  end

  def update_log(nr, string)
    nr.runlog += string + "\n"
    nr.save
  end

end
