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
require 'uri'

class BarclampIpmi::WsmanHammer < Hammer

  def self.probe(node)
    # For now, restrict ourselves to running on a Dell IDRAC 7.
    # We will need to be more flexible in the future.
    return false unless BarclampIpmi::IpmiHammer.probe(node)
    address = NetworkAllocation.node_cat(node, "bmc").first.address.addr
    if Attrib.get('ipmi-mfgr-id',node) == "674" &&
        Attrib.get('ipmi-product-id',node) == "256 (0x0100)" &&
        Attrib.get('ipmi-device-id',node) == "32"
      return "https://#{address}/wsman"
    else
      return false
    end
  end

  def default_opts
    r = Openwsman::ClientOptions.new
    r.set_dump_request
    return r
  end
    
  
  def client
    return @client if @client
    uri = URI::parse(endpoint)
    scheme = uri.scheme
    host = uri.hostname
    port = uri.port
    path = uri.path
    @client = Openwsman::Client.new(host,port,path,scheme,username,authenticator)
    @client.encoding = "utf-8"
    if scheme == "https"
      @client.transport.verify_peer = 0
      @client.transport.verify_host = 0
    end
    @client.dumpfile=File.open("/var/log/rebar/wsman_xml.log",File::CREAT|File::APPEND|File::WRONLY)
    @client
  end

  def actions
    { power: [:status,:on?,:on,:off,:cycle,:reset,:halt],
      run: [:identify, :enumerate, :enumerate_epr, :invoke]
    }
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
    node.update!(alive: false) if power_state_to(8)
  end

  def cycle
    node.update!(alive: false) if power_state_to(on? ? 5 : 2)
  end

  def reset
    node.update!(alive: false) if power_state_to(on? ? 10 : 2)
  end

  def halt
    return unless on?
    node.update!(alive: false) if power_state_to(12)
  end

  # WSMAN helper methods that we expose for other OOB tasks
  def identify
    res = client.identify(self.default_opts)
    raise client.fault_string if res.fault?
    res
  end

  def enumerate(resource,filter=nil,opts = self.default_opts)
    opts.flags = Openwsman::FLAG_ENUMERATION_OPTIMIZATION
    opts.max_elements = 999
    res = client.enumerate(opts,filter,resource)
    raise client.fault_string if res.fault?
    res
  end

  def enumerate_epr(resource,filter=nil,opts = self.default_opts)
    opts.add_option("EnumerationMode","EnumerateEPR")
    enumerate(resource,filter,opts)
  end

  def invoke(resource,meth,args=nil,opts = self.default_opts)
    # We need to pick the power controller we will use.
    # For now, be stupid and pick the first one.
    epr = enumerate_epr(resource)
    epr.Items[0].each do |i|
      next if i.name.to_s == "ElementName"
      Rails.logger.debug("Adding invoke selector #{i.name.to_s}: #{i.text.to_s}")
      opts.add_selector(i.name.to_s,i.text.to_s)
    end
    res = nil
    if args
      xml = Openwsman::XmlDoc.new("#{meth}_INPUT",resource)
      args.each do |k,v|
        if v.is_a?(Array)
          v.each do |val|
            xml.root.add(resource,k.to_s,val.to_s)
          end
        else
          xml.root.add(resource,k.to_s,v.to_s)
        end
      end
      res = client.invoke(opts,resource,meth,xml)
      raise("Fault! #{resource}:#{meth}:#{xml.to_xml}\n => #{client.fault_string}\n#{res.to_xml}") if res.fault?
    else
      res = client.invoke(opts,resource,meth)
      raise("Fault! #{resource}:#{meth}\n => #{client.fault_string}\n#{res.to_xml}") if res.fault?
    end
    res
  end

  def __perform(meth,*args)
    res = send(meth,*args)
    res.respond_to?(:to_xml) ? res.to_xml : res
  end

  def power_state_to(k)
    resource = "http://schemas.dell.com/wbem/wscim/1/cim-schema/2/DCIM_CSPowerManagementService"
    meth = "RequestPowerStateChange"
    args = {"PowerState" => k}
    doc = invoke(resource,meth,args)
    doc.ReturnValue.text == "0"
  end

end
