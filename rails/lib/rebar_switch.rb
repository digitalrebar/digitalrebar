# Copyright 2015, Greg Althaus
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
#

class RebarSwitch

  def self.map
    switches = {}
    Node.all.each do |n|
      val = Attrib.get('ports', n)
      if val
        if val['status'] == 'Success'
          val['lldp'].keys.each do |intf|
            name = val['lldp'][intf]['chassis']['name'] 
            mac = val['lldp'][intf]['chassis']['mac'] 
            pi = val['lldp'][intf]['port']
            sindex = name || mac

            switches[sindex] = {} unless switches[sindex]
            switches[sindex]['name'] = name
            switches[sindex]['mac'] = mac
            switches[sindex]['mgmt-ip'] = val['lldp'][intf]['chassis']['mgmt-ip']
            switches[sindex]['descr'] = val['lldp'][intf]['chassis']['descr']

            switches[sindex]['ports'] = {} unless switches[sindex]['ports']
            ports = switches[sindex]['ports']

            pname = pi['ifname'] || pi['name'] || pi['descr']
            ports[pname] = { name: n.name, port: intf, descr: (pi['descr'] || "Unknown") }

          end
        else
          # GREG: undo switch stuff if it is persistent.
        end
      end
    end

    pp switches
  end

end

