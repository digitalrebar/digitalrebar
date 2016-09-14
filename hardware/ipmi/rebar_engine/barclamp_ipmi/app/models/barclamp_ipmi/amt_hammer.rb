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

class BarclampIpmi::AmtHammer < Hammer

  def self.probe(node)
    invoke("probe")
  end

  def actions
    { power: [:status, :on?, :on, :off, :cycle],
      run: [:invoke]
    }
  end

  def status
    out, res = invoke("powerState")
    !!(out.strip =~ /on$/) ? "on" : "off"
  end

  def on?
    status == "on"
  end

  def on
    return if on?
    _, res = invoke("powerOn")
    res
  end

  def off
    return unless on?
    _, res = invoke("powerOff")
    res
  end

  def cycle
    if on?
      off
      while on? do
        sleep 1
      end
    end
    on
  end
  
    
  def invoke(*args)
    cmd = "amttool -e #{endpoint} -u #{username} -p #{authenticator} -op #{args.map{|a|a.to_s}.join(' ')}"
    res = %x{#{cmd} 2>&1}
    return [res, $?.exitstatus == 0]
  end
end
    
