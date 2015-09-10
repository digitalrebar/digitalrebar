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

# This class encapsulates everything we can do w.r.t power state managment
# on a node via IPMI

class BarclampIpmi::IpmiHammer < Hammer

  # See if a node can be managed via IPMI.
  # To be manageable via IPMI, it must have an ipmi-configure role bound to
  # it that has sucessfully configured.
  def self.probe(node)
    (Attrib.get('ipmi-configured',node) rescue nil)
  end

  def actions
    { power: [:status,:on?,:on,:off,:cycle,:reset,:halt,:pxeboot,:identify],
      run: [:invoke],
      boot: [:pxe, :disk]
    }
  end

  # Whether the node is powered on or not.
  def status
    out, res = invoke("chassis power status")
    !!(out.strip =~ / on$/) ? "on" : "off"
  end

  # As above, but in convienent boolean form.
  def on?
    status == "on"
  end

  # Turn a node on.
  def on
    return if on?
    out,res = invoke("chassis power on")
    !!(out.strip =~ /Up\/On$/)
  end

  # Power the node off.  This is an immediate action, the
  # OS will not have a chance to clean up.
  def off
    out,res = invoke("chassis power off")
    node.update!(alive: false) if out.strip =~ /Down\/Off$/
  end

  # Turn the node off, and then back on again.
  # If the node is already off, this just turns it back on.
  def cycle
    return on unless on?
    out,res = invoke("chassis power cycle")
    node.update!(alive: false) if out.strip =~ /Cycle$/
  end

  # Hard-reboot the node without powering it off.
  def reset
    return on unless on?
    out,res = invoke("chassis power reset")
    node.update!(alive: false) if out.strip =~ /Reset$/
  end

  # Gracefully power the node down.
  # If there is an OS invokening on the node, it will be asked to
  # power the node down.
  def halt
    return unless on?
    out,res = invoke("chassis power soft")
    node.update!(alive: false) if out.strip =~ /Soft$/
  end

  # Force the node to PXE boot.  IF the node is turned on, it
  # will be powercycled.
  def pxeboot
    out,res = invoke("chassis bootparam set bootflag force_pxe")
    cycle if out.strip =~ /force_pxe$/
  end

  # Have the node PXE boot first from now on.
  def pxe
    out,res = invoke("chassis bootdev pxe options=persistent")
  end

  # Have the node disk boot first from now on.
  def disk
    out,res = invoke("chassis bootdev disk options=persistent")
  end

  # Cause the identification lamp on the node to blink.
  # Right now, we only blink for 255 seconds.
  def identify
    invoke("chassis identify 255")
  end

  def invoke(*args)
    sleep(5) unless node.quirks.member?("ipmi-nodelay")
    @lanproto ||= Attrib.get('ipmi-version',node).to_f >= 2.0 ? "lanplus" : "lan"
    cmd = "ipmitool -I #{@lanproto} -U #{username} -P #{authenticator} -H #{endpoint} #{args.map{|a|a.to_s}.join(' ')}"
    res = %x{ #{cmd} 2>&1}
    return [res, $?.exitstatus == 0]
  end
end
