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

class BarclampRaid::ToolsInstall < Role

  def on_active(nr)
    enabled = Attrib.get('enable-raid-subsystem',nr.node)
    Rails.logger.fatal("GREG: here: #{enabled}")
    unless enabled
      update_log(nr, "enable-raid-subsystem not enabled, skipping raid hammer creation")
      return
    end

    unless nr.node.hammers.find_by(type: 'BarclampRaid::RaidHammer')
      Hammer.bind(manager_name: 'raid-hammer',
                  username: '',
                  authenticator: '',
                  node: nr.node)
    end
  end

  def update_log(nr, string)
    nr.runlog += string + "\n"
    nr.save
  end

end

