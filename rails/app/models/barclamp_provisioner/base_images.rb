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
#

class BarclampProvisioner::BaseImages < Role

  def sysdata(nr)
    return {
      "rebar" => {
        "provisioner" => {
          "barclamps" => Barclamp.all.map{|bc|bc.cfg_data}
        }
      }
    }
  end

  def on_active(nr)
    # Make sure that the default is in the list, if not reset it to the first entry.
    oses = Attrib.get("provisioner-available-oses", nr.node).map{ |k,v| k } rescue []
    default = Attrib.get('provisioner-target_os', Role.find_by(name: 'provisioner-os-install')) rescue "fred"
    # If oses is not empty and oses does not include default
    unless oses.empty? or oses.include?(default)
      Attrib.set('provisioner-target_os', Role.find_by(name: 'provisioner-os-install'), oses[0])
    end
  end
end
