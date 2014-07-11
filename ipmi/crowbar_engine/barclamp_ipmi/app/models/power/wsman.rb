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
# on a node via WSMAN.  It assumes that the node is a Dell server.

require 'openwsman'

class Power::WSMAN < Power

  def self.probe(node)
    # For now, restrict ourselves to running on a Dell IDRAC 7.
    # We will need to be more flexible in the future.
    Power::IPMI.probe(node) &&
      Attrib.get('ipmi-mfgr-id',node) == "674" &&
      Attrib.get('ipmi-product-id',node) == "256 (0x0100)" &&
      Attrib.get('ipmi-device-id',node) == "32"
  end

  def self.priority
    2
  end

  def initialize(node)
    raise "#{node.name} does not have a configured bmc!" unless self.class.probe(node)
    @node = node
    @address = Network.address(node: node, network: "bmc", range: "host").address.addr
    @port = 443
    @scheme = "https"
    @prefix = "/wsman"
    @username = Attrib.get('ipmi-username',node)
    @password = Attrib.get('ipmi-password',node)
    @node = node
    @client = Openwsman::Client.new(@address,@port,@prefix,@scheme,@username,@password)
    if @scheme == "https"
      @client.transport.verify_peer = 0
      @client.transport.verify_host = 0
    end
    @client.encoding = "utf-8"
  end

  def status
    resource = "http://schemas.dell.com/wbem/wscim/1/cim-schema/2/DCIM_CSAssociatedPowerManagementService"
    doc = enumerate(resource)
    doc.PowerState.text == "2" ? "on" : "off"
  end

  def on?
    status == "on"
  end

  def on
    return if on?
    power_state_to(2)
  end

  def off
    return unless on?
    @node.update!(alive: false) if power_state_to(8)
  end

  def cycle
    @node.update!(alive: false) if power_state_to(on? ? 5 : 2)
  end

  def reset
    @node.update!(alive: false) if power_state_to(on? ? 10 : 2)
  end

  def halt
    return unless on?
    @node.update!(alive: false) if power_state_to(12)
  end

  private

  def identifu
    res = @client.identify(Openwsman::ClientOptions.new)
    raise @client.fault_string if res.fault?
    res
  end

  def enumerate(resource,filter=nil,opts = Openwsman::ClientOptions.new)
    opts.flags = Openwsman::FLAG_ENUMERATION_OPTIMIZATION
    opts.max_elements = 999
    res = @client.enumerate(opts,filter,resource)
    raise @client.fault_string if res.fault?
    res
  end

  def enumerate_epr(resource,filter=nil,opts = Openwsman::ClientOptions.new)
    opts.add_option("EnumerationMode","EnumerateEPR")
    enumerate(resource,filter,opts)
  end

  def invoke(resource,meth,args=nil,opts = Openwsman::ClientOptions.new)
    xml = Openwsman::XmlDoc.new("#{meth}_INPUT",resource)
    args.each do |k,v|
      xml.root.add(resource,k.to_s,v.to_s)
    end if args
    res = @client.invoke(opts,resource,meth,xml)
    raise @client.fault_string if res.fault?
    res
  end

  def power_state_to(k)
    resource = "http://schemas.dell.com/wbem/wscim/1/cim-schema/2/DCIM_CSPowerManagementService"
    meth = "RequestPowerStateChange"
    args = {"PowerState" => k}
    opts = Openwsman::ClientOptions.new
    # We need to pick the power controller we will use.
    # For now, be stupid and pick the first one.
    epr = enumerate_epr(resource)
    epr.Items[0].each do |i|
      opts.add_selector(i.name.to_s,i.text.to_s)
    end
    doc = invoke(resource,meth,args,opts)
    doc.ReturnValue.text == "0"
  end

end
