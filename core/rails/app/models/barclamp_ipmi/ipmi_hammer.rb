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
    Rails.logger.debug("IPMI: status: start")
    out, res = invoke("chassis power status")
    Rails.logger.debug("IPMI: status: result: #{out}")
    !!(out.strip =~ / on$/) ? "on" : "off"
  end

  # As above, but in convienent boolean form.
  def on?
    status == "on"
  end

  # Turn a node on.
  def on
    Rails.logger.debug("IPMI: on: start")
    return if on?
    Rails.logger.debug("IPMI: on: action")
    out,res = invoke("chassis power on")
    Rails.logger.debug("IPMI: on: result: #{out}")
    !!(out.strip =~ /Up\/On$/)
  end

  # Power the node off.  This is an immediate action, the
  # OS will not have a chance to clean up.
  def off
    Rails.logger.debug("IPMI: off: start")
    out,res = invoke("chassis power off")
    Rails.logger.debug("IPMI: off: result: #{out}")
    node.update!(alive: false) if out.strip =~ /Down\/Off$/
  end

  # Turn the node off, and then back on again.
  # If the node is already off, this just turns it back on.
  def cycle
    Rails.logger.debug("IPMI: cycle: start")
    return on unless on?
    Rails.logger.debug("IPMI: cycle: action")
    out,res = invoke("chassis power cycle")
    Rails.logger.debug("IPMI: cycle: result")
    node.update!(alive: false) if out.strip =~ /Cycle$/
  end

  # Hard-reboot the node without powering it off.
  def reset
    Rails.logger.debug("IPMI: reset: start")
    return on unless on?
    Rails.logger.debug("IPMI: reset: action")
    out,res = invoke("chassis power reset")
    Rails.logger.debug("IPMI: reset: result: #{out}")
    node.update!(alive: false) if out.strip =~ /Reset$/
  end

  # Gracefully power the node down.
  # If there is an OS invokening on the node, it will be asked to
  # power the node down.
  def halt
    Rails.logger.debug("IPMI: halt: start")
    return unless on?
    Rails.logger.debug("IPMI: halt: action")
    out,res = invoke("chassis power soft")
    Rails.logger.debug("IPMI: halt: result #{out}")
    node.update!(alive: false) if out.strip =~ /Soft$/
  end

  # Force the node to PXE boot.  IF the node is turned on, it
  # will be powercycled.
  def pxeboot
    Rails.logger.debug("IPMI: pxeboot: start")
    out,res = invoke("chassis bootparam set bootflag force_pxe")
    Rails.logger.debug("IPMI: pxeboot: result: #{out}")
    cycle if out.strip =~ /force_pxe$/
  end

  # Have the node PXE boot first from now on.
  def pxe
    Rails.logger.debug("IPMI: pxe: start")
    out,res = invoke("chassis bootdev pxe options=persistent")
    Rails.logger.debug("IPMI: pxe: result #{out}")
  end

  # Have the node disk boot first from now on.
  def disk
    Rails.logger.debug("IPMI: disk: start")
    out,res = invoke("chassis bootdev disk options=persistent")
    Rails.logger.debug("IPMI: disk: result #{out}")
  end

  # Cause the identification lamp on the node to blink.
  # Right now, we only blink for 255 seconds.
  def identify
    Rails.logger.debug("IPMI: identifiy: start")
    invoke("chassis identify 255")
    Rails.logger.debug("IPMI: identifiy: result #{out}")
  end

  def invoke(*args)
    Rails.logger.debug("IPMI: invoke: start")
    @lanproto ||= Attrib.get('ipmi-version',node).to_f >= 2.0 ? "lanplus" : "lan"
    cmd = "ipmitool -I #{@lanproto} -U #{username} -P #{authenticator} -H #{endpoint} #{args.map{|a|a.to_s}.join(' ')}"
    res = %x{ #{cmd} 2>&1}
    Rails.logger.debug("IPMI: invoke: result: #{res}")
    return [res, $?.exitstatus == 0]
  end
end
