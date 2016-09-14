# Copyright 2014, Victor Lowther
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Delegate run calls back to a role provided do_transition method.
# This jig should be used when the behavior a role implements is:
#   * out of band, and
#   * So specific to that role that abstracting its behaviour into a jig
#     would not result in any significant streamlining.
class BarclampRebar::RoleProvidedJig < Jig

  def run(nr,data)
    nr.role.do_transition(nr,data)
  end

end
