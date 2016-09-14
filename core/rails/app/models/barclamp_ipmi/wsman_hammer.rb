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

require 'xmlsimple'

class BarclampIpmi::WsmanHammer < Hammer

  class WsmanCliWrapper

    class WsmanResult
      def initialize(data, fault=false, fs="Unknown")
        @data = data
        @fault = fault
        @fault_string = fs
      end

      def data
        @data
      end

      def to_xml
        @data.to_xml
      end

      def keys
        @data.keys
      end

      def recurse_map(ans)
        if ans.is_a? Hash
          WsmanResult.new(ans, @fault, @fault_string)
        elsif ans.is_a? Array
          ans.map { |x| recurse_map(x) }
        else
          ans
        end
      end

      def method_missing(method_sym, *arguments, &block)
        # the first argument is a Symbol, so you need to_s it if you want to pattern match
        if arguments.length == 0
          recurse_map(@data[method_sym.to_s])
        else
          super
        end
      end

      def fault?
        @fault
      end
      def fault_string
        @fault_string
      end
    end

    def initialize(e, u, p)
      @endpoint = e
      @username = u
      @password = p
    end

    def run_cli(action, resource = nil, method = nil, args = nil, opts = {})

      res = ''
      res = "-r '#{resource}'" if resource
      meth = ''
      meth = "-m '#{method}'" if method

      if opts['selector']
        selector = '-s \''
        first = true
        opts['selector'].each do |k,v|
          selector = selector + ',' unless first
          selector = selector + "#{k}: #{v}"
          first = false
        end
        selector = selector + '\''
      else
        selector = ''
      end

      data = ''
      if args
        data = '-x \''
        first = true
        args.each do |k,v|
          if v.is_a? Array
            v.each do |subv|
              data = data + ',' unless first
              data = data + "#{k}: #{subv}"
              first = false
            end
          else
            data = data + ',' unless first
            data = data + "#{k}: #{v}"
            first = false
          end
        end
        data = data + '\''
      end

      optimize = ''
      optimize = '-q' if opts['optimize']

      out,err = '',''
      cmd = %Q{/go/bin/wscli -e '#{@endpoint}' -u '#{@username}' -p '#{@password}' -a #{action} #{optimize} #{res} #{meth} #{selector} #{data}}
      Rails.logger.debug(cmd)
      status = Open4::popen4ext(true,cmd) do |pid,stdin,stdout,stderr|
        stdin.close

        begin
          # Readpartial will read all the data up to 16K
          # If not data is present, it will block
          loop do
            out << stdout.readpartial(16384)
          end
        rescue Errno::EAGAIN
          retry
        rescue EOFError
        end

        err << stderr.read
        stdout.close
        stderr.close
      end

      Rails.logger.debug("response #{status.exitstatus} = #{out}")

      res = case status.exitstatus
      when 0
        WsmanResult.new(XmlSimple.xml_in(out, { 'ForceArray' => false, 'ForceContent' => true, 'ContentKey' => 'text' }))
      when 1
        # SOAP Fault
        WsmanResult.new(XmlSimple.xml_in(out, { 'ForceArray' => false, 'ForceContent' => true, 'ContentKey' => 'text' }), true, "SOAP Fault")
      else
        WsmanResult.new({}, true, "Command Error: #{out}\n #{err}")
      end

      Rails.logger.fatal("returning = #{res}")
      res
    end

    def identify
      run_cli('Identify')
    end

    def enumerate(resource, filter = nil, opts = {})
      opts['optimize'] = true
      res = run_cli('Enumerate', resource)
      unless res.fault?
        last_part = resource.split('/').last
        tmp = res.Body.EnumerateResponse.Items.data[last_part]
        newhash = { "Items" => tmp }
        res = res.recurse_map(newhash)
      end
      res
    end

    def enumerate_epr(resource, filter = nil, opts = {})
      res = run_cli('EnumerateEPR', resource)
      unless res.fault?
        tmp = res.Body.EnumerateResponse.Items.EndpointReference.ReferenceParameters.SelectorSet.Selector
        newhash = { "Items" => tmp }
        res = res.recurse_map(newhash)
      end
      res
    end

    def invoke(resource, meth, args = nil, opts = {})
      res = run_cli('Invoke', resource, meth, args, opts)
      unless res.fault?
        wrapper = res.Body.keys.first
        tmp = res.Body.data[wrapper]
        res = res.recurse_map(tmp)
      end
      res
    end
  end

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

  def client
    return @client if @client
    @client = WsmanCliWrapper.new(endpoint,username,authenticator)
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
    res = client.identify
    raise res.fault_string if res.fault?
    res
  end

  def enumerate(resource,filter=nil,opts = {})
    res = client.enumerate(resource,filter,opts)
    raise res.fault_string if res.fault?
    res
  end

  def enumerate_epr(resource,filter=nil,opts = {})
    res = client.enumerate_epr(resource,filter,opts)
    raise res.fault_string if res.fault?
    res
  end

  def invoke(resource,meth,args=nil,opts = {})
    # We need to pick the power controller we will use.
    # For now, be stupid and pick the first one.
    epr = enumerate_epr(resource)
    epr.Items.each do |i|
      Rails.logger.debug("Adding invoke selector #{i.Name.to_s}: #{i.text.to_s}")
      opts['selector'] ||= {}
      opts['selector'][i.Name.to_s] = i.text.to_s
    end
    res = client.invoke(resource,meth,args,opts)
    raise("Fault! #{resource}:#{meth}\n => #{res.fault_string}\n#{res.to_xml}") if res.fault?
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
