#!/usr/bin/env ruby

require 'rubygems'
require 'xmlsimple'
require 'yaml'
require 'json'

class Rebar 
 class WSMAN
  attr :host
  attr :user
  attr :password
  attr :port
  attr :debug_time

  def self.certname(host)
    "/tmp/cer-#{host}.cer"
  end

  def self.setup_env(host, user, password)
    filename = WSMAN.certname(host)
    return true if File.exists?(filename)

    output = %x{ping -W 3 -c 2 #{host} 2>/dev/null >/dev/null}
    if $?.exitstatus != 0 
      Chef::Log.error "Failed to ping host: #{host}"
      return false
    end
  
    output = %x{echo | openssl s_client -connect #{host}:443 2>&1 | sed -ne '/-BEGIN CERTIFICATE-/,/-END CERTIFICATE-/p' >#{filename} 2>&1}
    if $?.exitstatus != 0
      Chef::Log.error output
      return false
    end
    true
  end

  def initialize(opts = {})
    @host = opts[:host]
    @user = opts[:user]
    @password = opts[:password]
    @port = opts[:port] || 443
    @debug_time = opts[:debug_time] || false
    @debug = opts[:debug] || false
  end

  def measure_time(msg)
    start = Time.now if @debug_time
    yield
    puts "#{msg}: #{Time.now - start}" if @debug_time
  end

  def setup_env
    WSMAN.setup_env(@host, @user, @password)
  end

  # Action = enumerate, invoke, ...
  # url = http://schemas.dmtf.org/wbem/wscim/1/cim-schema/2/DCIM_SoftwareIdentity
  # args = non-standard args (not -h, -P, -u, -p, -c)
  def command(action, url, args = "", count = 0)
    self.setup_env
    filename = WSMAN.certname(@host)
    output = ""
    ret=0
    stdargs = "-N root/dcim -v -o -j utf-8 -y basic"
    self.measure_time "WSMAN #{action} #{url} call" do
      cmd = "wsman #{action} #{url} -h #{@host} -P #{@port} -u #{@user} -p #{@password} -c #{filename} #{stdargs} #{args} 2>&1"
      output = %x{#{cmd}}
      ret = $?.exitstatus
    end
    if ret != 0
      Chef::Log.error "wsman command failed: #{action}"
      Chef::Log.error output
      return false
    end

    # Retry this three times
    if output =~ /Connection failed. response code = 0/
      return false if count >= 3
      puts "Retrying the command: #{count} #{action}"
      sleep 20
      return command(action, url, args, count + 1)
    end

    return output
  end

  #
  # time is:
  #   YYYYMMDDHHmmSS
  #   TIME_NOW
  #
  def schedule_job(jid, time)
    output = self.command("invoke -a SetupJobQueue", 
                          "http://schemas.dmtf.org/wbem/wscim/1/cim-schema/2/DCIM_JobService?CreationClassName=DCIM_JobService,Name=JobService,SystemName=Idrac,SystemCreationClassName=DCIM_ComputerSystem", 
                          "-k JobArray=\"#{jid}\" -k StartTimeInterval=\"#{time}\"")
    return false unless output

    hash = XmlSimple.xml_in(output, "ForceArray" => false)
    t = hash["Body"]
    if t["Fault"]
      return false, t["Fault"]
    end

    # Some versions don't actually give a return code on success.
    if (t["SetupJobQueue_OUTPUT"]["ReturnValue"].instance_of? Hash)
      return true, "nil value" 
    end

    if t["SetupJobQueue_OUTPUT"]["ReturnValue"] != "0"
      return false, t["SetupJobQueue_OUTPUT"]["Message"]
    end

    return true, 0
  end

  def clear_all_jobs
    output = self.command("invoke -a DeleteJobQueue", 
                          "http://schemas.dmtf.org/wbem/wscim/1/cim-schema/2/DCIM_JobService?CreationClassName=DCIM_JobService,Name=JobService,SystemName=Idrac,SystemCreationClassName=DCIM_ComputerSystem", 
                          "-m 256 -k JobID=\"JID_CLEARALL\"")
    return false unless output

    hash = XmlSimple.xml_in(output, "ForceArray" => false)
    t = hash["Body"]
    if t["Fault"]
      return false, t["Fault"]
    end

    # Some versions don't actually give a return code on success.
    if (t["DeleteJobQueue_OUTPUT"]["ReturnValue"].instance_of? Hash)
      return true, "nil value" 
    end
    ret = t["DeleteJobQueue_OUTPUT"]["ReturnValue"].to_i == 0 rescue false
    return ret, t["DeleteJobQueue_OUTPUT"]["Message"]
  end

  def get_job_status(jid)
    output = self.command("get", 
                          "http://schemas.dmtf.org/wbem/wscim/1/cim-schema/2/DCIM_LifecycleJob?InstanceID=#{jid}")
    return false unless output

    hash = XmlSimple.xml_in(output, "ForceArray" => false)
    t = hash["Body"]
    if t["Fault"]
      return false, t["Fault"]["Reason"]["Text"]["content"]
    end

    # Sometime the job will hang 
    js = t["DCIM_LifecycleJob"]["JobStatus"]
    if js == "New"
      if t["DCIM_LifecycleJob"]["MessageID"] == "RED023"
        return false, "In Use"
      end
    end

    return true, js
  end

  def is_RS_ready?
    output = self.command("invoke -a GetRSStatus", 
                          "http://schemas.dmtf.org/wbem/wscim/1/cim-schema/2/root/dcim/DCIM_LCService?SystemCreationClassName=DCIM_ComputerSystem,CreationClassName=DCIM_LCService,SystemName=DCIM:ComputerSystem,Name=DCIM:LCService",
                          "-V")
    return false unless output

    hash = XmlSimple.xml_in(output, "ForceArray" => false)
    t = hash["Body"]
    if t["Fault"]
      return false, t["Fault"]
    end

    # Some versions don't actually give a return code on success.
    if (t["GetRSStatus_OUTPUT"]["ReturnValue"].instance_of? Hash)
      return true, "Ready"
    end

    if t["GetRSStatus_OUTPUT"]["ReturnValue"] != "0"
      return false, t["GetRSStatus_OUTPUT"]["Message"]
    end

    status = t["GetRSStatus_OUTPUT"]["Status"]
    return status == "Ready", status
  end

 end
end
