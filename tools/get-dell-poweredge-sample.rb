#!/usr/bin/env ruby
$username = 'root'
$password = 'calvin'
$endpoint = "https://192.168.0.1/wsman"

require "rexml/document"
require "optparse"
require "open3"
require "json"
def log(str)
  $stderr.puts(str)
end

def enumerate(resource)  
  res = "-r '#{resource}'"
  out,err = '',''
  cmd = %Q{wscli -e '#{$endpoint}' -u '#{$username}' -p '#{$password}' -a Enumerate -q #{res}}
  out, err, status = Open3::capture3(cmd)
  
  case status.exitstatus
  when 0
    REXML::Document.new(out)
  when 1
    log "SOAP fault"
    log out
    exit 1
  else
    if err =~ /wscli/
      log "wscli not installed"
      log "Install it with:"
      log "  go get https://github.com/VictorLowther/wsman/tree/master/wscli"
      log "and then make sure $GOPATH/bin is in your $PATH"
      exit 1
    end
    log "Command error"
    log out
    log err
    exit 1
  end
end

OptionParser.new do |opts|
  opts.banner = "Usage: get-dell-poweredge-sample.rb [options]"
  opts.on("-e", "--endpoint ENDPOINT", "The WSMAN endpoint on the iDRAC to talk to") do |ep|
    $endpoint = ep
  end
  opts.on("-u", "--username USERNAME", "The WSMAN username on the iDRAC") do |user|
    $username = user
  end
  opts.on("-p", "--password PASSWORD", "The WSMAN password on the iDRAC") do |pw|
    $password = pw
  end
end.parse!

res = {}

elems = [
 enumerate("http://schemas.dell.com/wbem/wscim/1/cim-schema/2/DCIM_BIOSEnumeration"),
 enumerate("http://schemas.dell.com/wbem/wscim/1/cim-schema/2/DCIM_BIOSString"),
 enumerate("http://schemas.dell.com/wbem/wscim/1/cim-schema/2/DCIM_BIOSInteger")]
elems.each do |e|
  e.elements.each('/s:Envelope/s:Body/wsen:EnumerateResponse/wsman:Items/*') do |elem|
    res[elem.text("n1:AttributeName")] = elem.text("n1:CurrentValue")
  end
end
log "These are the current values of the settings, not the defaults."
log "The WSMAN interface Dell provides does not have a method of getting the"
log "default value of a setting without resetting the whole system to factory defaults"
log ""
log "You should also clean these values to eliminate ones that cannot change."
log "Refer to the DCIM BIOS and Boot Management profile for the system at:"
log " http://en.community.dell.com/techcenter/systems-management/w/wiki/1906.dcim-library-profile"
log ""
puts JSON.pretty_generate({"default" => res})
