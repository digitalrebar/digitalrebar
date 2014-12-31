# Copyright 2011, Dell
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

# This recipie is a placeholder for misc. hacks we want to do on every node,
# but that do not really belong with any specific barclamp.

states = [ "ready", "readying", "recovering", "applying" ]
if states.include?(node[:state])
  # Don't waste time with mlocate or updatedb
  %w{mlocate mlocate.cron updatedb}.each do |f|
    file "/etc/cron.daily/#{f}" do
      action :delete
    end
  end

  # Set up some basic log rotation
  template "/etc/logrotate.d/crowbar-webserver" do
    source "logrotate.erb"
    owner "root"
    group "root"
    mode "0644"
    variables(:logfiles => "/opt/dell/crowbar_framework/log/*.log",
                :action => "create 644 crowbar crowbar",
              :postrotate => "/usr/bin/killall -USR1 puma")
  end if node[:recipes].include?("crowbar")
  template "/etc/logrotate.d/node-logs" do
    source "logrotate.erb"
    owner "root"
    group "root"
    mode "0644"
    variables(:logfiles => "/var/log/nodes/*.log",
              :postrotate => "/usr/bin/killall -HUP rsyslogd")
  end if node[:recipes].include?("logging::server")
  template "/etc/logrotate.d/client-join-logs" do
    source "logrotate.erb"
    owner "root"
    group "root"
    mode "0644"
    variables(:logfiles => ["/var/log/crowbar-*.log","/var/log/crowbar-*.err"])
  end unless node[:recipes].include?("crowbar")
end

# This should really be its own recipe, but...
if File.exists?("/sys/firmware/efi")
  bootargs = Mash.new
  bootargs["Entries"] = Array.new
  IO.popen("efibootmgr -v") do |p|
    p.each do |line|
      res = nil
      k,v = line.split(' ',2)
      k.gsub!(/[:]$/,'')
      v.strip!
      case
      when ['BootCurrent','BootNext'].member?(k) then bootargs[k] = v.hex
      when k == 'BootOrder' then bootargs[k] = v.split(',').map{|i|i.hex}
      when k =~ /^Boot[0-9a-fA-F]{1,4}/
        desc,device = v.split("\t")
        res = {}
        res["Description"] = desc.dup.freeze
        res["Device"] = device.dup.freeze
        res["Active"] = (k[-1,1] == '*')
        bootargs["Entries"][k.match(/^Boot([0-9a-fA-F]+)/)[1].hex] = res.dup.freeze
      else next
      end
    end
  end
  unless bootargs.empty? || bootargs["Entries"].empty?
    if bootargs["Entries"][bootargs["BootCurrent"]]["Device"] =~ /[\/)]MAC\(/i
      macaddr = bootargs["Entries"][bootargs["BootCurrent"]]["Device"].match(/[\/)]MAC\(([0-9a-f]+)/i)[1]
      bootargs["LastNetBootMac"] = ''
      6.times do |i|
        bootargs["LastNetBootMac"] << "#{macaddr[(i*2),2]}:"
      end
      bootargs["LastNetBootMac"].chop!
    end
    node.normal[:crowbar_wall] ||= Mash.new
    node.normal[:crowbar_wall][:uefi] ||= Mash.new
    node.normal[:crowbar_wall][:uefi][:boot] = bootargs
    bootargs["BootOrder"].each do |e|
      next if bootargs["Entries"][e]["Active"]
      Chef::Log.info("Activating UEFI boot entry #{sprintf('%x',e)}: #{bootargs["Entries"][e]["Description"]}")
      ::Kernel.system("efibootmgr -a -b #{sprintf('%x',e)}")
    end
  end
end
