# Copyright 2016 Rackn
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

class BarclampIpmi::AmtConfigure < Role

  def do_transition(nr, data)
    if nr.node.hammers.find_by(name: 'amt')
      nr.runlog = "AMT already configured"
      return
    end
    unless data['amt']['enable']
      nr.runlog = "AMT not enabled"
      return
    end
    ip = data['provider']['control_address']
    endpoint = "http://#{ip.split('/')[0]}:16992/wsman"
    username = "admin"
    password = data['amt']['password']
    res = false
    h = Hammer.bind(manager_name: 'amt',
                    username: username,
                    authenticator: password,
                    endpoint: endpoint,
                    node: nr.node)
    5.times do |t|
      _, res = h.invoke('probe')
      break if res
      nr.runlog << "Unable to access AMT at #{endpoint} (#{password})"
      sleep t
    end
    unless res
      h.destroy
      Attrib.set('amt-enable',nr.node,false)
      return
    end
    out, res = h.invoke('netState')
    if !res
      nr.runlog = "Unable to get network status at #{ip}"
      h.destroy
      Attrib.set('amt-enable',nr.node,false)
      return
    end
    cmd_line = "netConfig"
    out.each_line do |line|
      p,v = line.split(':',2)
      case p.strip
      when 'nic' then cmd_line << " -nic #{v.strip}"
      when 'ip' then cmd_line << " -ip #{v.strip}"
      when 'mask' then cmd_line << " -mask #{v.strip}"
      when 'gw' then cmd_line << " -gw #{v.strip}"
      when 'dns1' then cmd_line << " -dns1 #{v.strip}"
      when 'dns2' then cmd_line << " -dns2 #{v.strip}"
      end
    end
    nr.runlog << "Setting AMT network parameters: #{cmd_line}"
    out, res = h.invoke(cmd_line)
    if !res
      nr.runlog << "\nfailed!\n#{out}"
      return false
    end
    nr.runlog << "\nconfigured.\n"

  end
  
end
