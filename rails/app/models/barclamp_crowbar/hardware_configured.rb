# Copyright 2014, Victor Lowther
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

class BarclampCrowbar::HardwareConfigured < Role

  # If we know how to turn the node back on, we have no children,
  # and crowbar-managed-node is not our target role,
  # then turn the node off for now.
  def on_active(nr)
    nr.node.power.off if nr.children.empty? &&
      nr.node.power[:on] &&
      !(nr.node.target_role_id && nr.node.target_role_id == nr.role_id)
  end
end
