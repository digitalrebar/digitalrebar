#!/usr/bin/ruby
# Copyright (c) 2016 Rackn
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# This file contains the logic needed to do BIOS configuration and
# updates for Dell Poweredge R-series gear.
# It assumes that we can talk to the IPMI controller embedded on the system,
# and that the system has WSMAN enabled.

# Implement everything needed to manage the attributes we care about on
# a Supermicro box using Supermicro Update Manager

require 'tempfile'

class BarclampBios::Supermicro < BarclampBios::Driver

  def initialize(node, nr)
    super(node, nr)
    @ipmi = @node.hammers.find_by!(name: 'ipmi')
    @sum = "/usr/local/bin/sum"
    unless File.executable?(@sum)
      raise "SuperMicro Update Manager not installed at #{@sum}"
    end
  end

  def enable_license(macaddr, license_key)
    @macaddr = macaddr
    @license_key = license_key
  end

  def settings
    return @settings if @settings
    res = Hash.new

    tmp = Tempfile.new("sum_bios")
    out, status = sum("-c GetCurrentBiosCfgTextFile --overwrite --file #{tmp.path}")
    if status != 0
      raise "Failed to get BIOS settings: #{out}"
    end
    lines = IO.readlines(tmp.path)
    tmp.close
    tmp.unlink
    header_re=/^\[(.+)\]$/
    setting_re=/^([^=]+)=([0-9a-fA-F]+)/
    option_re=/\*?(([0-9a-fA-F]+) \(.+)/
    header = ''
    lines.each do |l|
      line = l.strip
      next if line.empty? || line[0] == '#'
      i = Hash.new
      case
      when header_re =~ line
        header = $~[1]
      when setting_re =~ line
        i["name"] = header + "|" + $~[1]
        i["driver"] = self
        i["validator"] = []
        i["current_value"] = $~[2]
        values = line.split("//",2)[1].split("        ",2)[0].strip.split('), ').map{|v|v.strip}
        values[-1].chop!
        values.each do |val|
          md = option_re.match(val)
          next unless md
          val = md[1] + ')'
          i["validator"] << val
          i["current_value"] = val if md[2] == i["current_value"]
        end
        if i["validator"].empty?
          i["validator"] = (0..65536)
          i["current_value"] = i["current_value"].to_i(16)
        end
          
        res[i["name"]] = BarclampBios::Setting.new(i)
      end
    end
    @settings = res
  end

  def commit
    to_commit = @settings.values.reject{|s|s.current_value == s.proposed_value || s.proposed_value.nil?}
    return false if to_commit.empty?
    buckets = Hash.new
    to_commit.each do |setting|
      vars = setting.name.split("|")
      header = vars[0..-2].join("|")
      var = vars[-1]
      buckets[header] ||= Hash.new
      case
      when setting.proposed_value.is_a?(Numeric)
        buckets[header][var] = setting.proposed_value.to_int.to_s(16)
      else
        buckets[header][var] = setting.split(' ')[0]
      end
    end
    tmp = Tempfile.new("sum_bios")
    buckets.each do |header, vals|
      tmp.puts "[#{header}]"
      vals.each do |k,v|
        tmp.puts "#{k}=#{v}"
      end
      tmp.puts ''
    end
    tmp.close
    out, status = sum("-c ChangeBiosCfg --file #{tmp.path} --reboot")
    if status != 0
      raise "Failed to change BIOS settings: #{out}"
    end
    tmp.unlink
    true
  end

  def flash(packages)
    true
  end

  def sum(cmd)
    res = %x{/usr/local/bin/sum -u #{@ipmi.username} -p #{@ipmi.authenticator} -i #{@ipmi.endpoint} #{cmd}}
    status = $?.exitstatus
    if status == 80
      # License not activated.  Activate it and try again.
      res, status = sum("-c ActivateProductKey --key #{@license_key}")
      if status != 0
        raise "Failed to activate license key for #{@macaddr}"
      end
      return sum(cmd)
    end
    return res, status
  end
end
