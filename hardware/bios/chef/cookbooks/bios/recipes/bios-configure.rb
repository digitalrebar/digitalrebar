#!/usr/bin/ruby
# Copyright (c) 2013 Dell Inc.
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

include_recipe "bios::bios-common"

def get_bag_item_safe (name, descr)
  data_bag_item("rebar-data", name)
rescue
  Chef::Log.error("couldn't find #{descr} named #{name}")
  node["rebar_wall"]["status"]["bios"] << "Could not find #{descr} named #{name}"
  nil
end

debug = node[:bios][:debug]
product = node[:dmi][:system][:product_name].gsub(/\s+/, '')
default_set = "bios-set-#{product}-default"
bios_set = get_bag_item_safe(default_set, "bios defaults")
bios_version = node[:dmi] && node[:dmi][:bios] && node[:dmi][:bios][:version]
style = bios_set && bios_set[:style] || "legacy"
style = "legacy" if style.length == 0
node[:bios][:style] = style
Chef::Log.info("Bios set style is: #{style}")

if !@@bios_setup_enable
  Chef::Log.info("Bios configuration disabled by user.")
elsif !bios_set
  Chef::Log.info("Unable to get default BIOS settings for #{product}")
  Chef::Log.info("Skipping BIOS configuration")
elsif (style != "wsman") && ! (
				bios_set[:versions] &&
				bios_set[:versions].kind_of?(Array) &&
				bios_set[:versions].member?(bios_version))
  Chef::Log.info("Skipping BIOS parameter setup for BIOS version #{bios_version}.")
  Chef::Log.info("For #{product}, we only know how to set parameters on BIOS versions #{bios_set[:versions].join(", ")}.")
else
  Chef::Log.info("Will configure #{product} BIOS #{bios_version} parameters")
  case style
  when "legacy","new_pec","unified_pec"
    # Run the statically linked version of setupbios on the ubuntu platform
    pgm_dir = "/opt/bios/setupbios"
    ENV['SETUPBIOS_SETTINGS_PATH']=pgm_dir
    pgmname = "#{pgm_dir}/alternate_version/setupbios.static"
  end

  ## try to get the per-role set name.
  ## look for role+platform specific, and if not found, use role only.
  ## if neither found, use just defualts.
  bios_set_name = node[:rebar][:hardware][:bios_set]
  setname = "bios-set-#{product}-#{bios_set_name}"
  bios_over = get_bag_item_safe(setname, " overrides for #{setname} ")
  if bios_over.nil?
    setname = "bios-set-#{bios_set_name}"
    bios_over = get_bag_item_safe(setname, " overrides for #{setname} ")
  end

  if bios_over.nil?
    log("no role overide settings, setting to defaults" ) { level :warn}
    values = bios_set["attributes"].dup
  else
    log("using role overide settings from: #{setname}") { level :warn}
    # WSMAN needs a deep merge!
    if style == "wsman"
      # overlay[fqdd][groups][attr_names] = values (value is a hash)
      bios_over["attributes"].each do |k, groups|
	if bios_set["attributes"][k]
	  groups.each do |group, attrs|
	    if bios_set["attributes"][k][group]
	      bios_set["attributes"][k][group] = bios_set["attributes"][k][group].merge(attrs)
	    else
	      bios_set["attributes"][k][group] = attrs
	    end
	  end
	else
	  bios_set["attributes"][k] = groups
	end
      end
      values = bios_set["attributes"]
    else
      values = bios_set["attributes"].merge(bios_over["attributes"])
    end
  end

  case  style
  when "wsman"
    # If we have a asset_tag entry, try and set it in the bios.
    if node["asset_tag"]
      values["BIOS"] = {} unless values["BIOS"]
      values["BIOS"]["default"] = {} unless values["BIOS"]["default"]
      values["BIOS"]["default"]["AssetTag"] = {
        "instance_id" => "BIOS.Setup.1-1:AssetTag",
        "value" => node["asset_tag"],
        "DefaultValue" => "",
        "attr_name" => "AssetTag",
        "GroupID" => nil
      }
    end
    bios_configure "wsman" do
      type           "wsman"
      product         node[:dmi][:system][:product_name]
      max_tries       node[:bios][:max_tries]
      values          values
      problem_file "/var/log/chef/hw-problem.log"
      action   :configure
    end
  when "unified_pec"
    need_reboot = false
    all_tokens_set = true
    node[:rebar_wall] ||= Mash.new
    node[:rebar_wall][:bios] ||= Mash.new
    node[:rebar_wall][:bios][:pec_symbolic_change_map] ||= Mash.new
    node[:rebar_wall][:bios][:pec_raw_change_map] ||= Mash.new
    symbolic_change_map = node[:rebar_wall][:bios][:pec_symbolic_change_map].to_hash
    raw_change_map = node[:rebar_wall][:bios][:pec_raw_change_map].to_hash
    # Split out raw tokens from symbolic tokens for the values we want to set.
    raw_tokens = []
    symbolic_tokens = {}
    values.each do |name,val|
      if name == "raw_tokens"
        raw_tokens = val
      else
        symbolic_tokens[name] = val
      end
    end
    # Get the current state of all the symbolic token settings.
    current_tokens = {}
    IO.popen("#{pgmname} setting save",'r') do |f|
      f.each do |raw_line|
        line = raw_line.gsub(/[#;].*/,'').strip.chomp
        next if line.empty?
        name,val = line.split(':',2).map{|t|t.strip}
        current_tokens[name] = val
      end
    end
    # Compare the current state of the BIOS to the state we want.
    # If any settings are not in the state we want, change them and
    # flag that we want a reboot.
    symbolic_tokens.each do |name,val|
      next if current_tokens[name] && (current_tokens[name] == val)
      if symbolic_change_map[name] && (symbolic_change_map[name][:tries] > 2)
        Chef::Log.error("Tried #{symbolic_change_map[name][:tries]} times to update #{name} from #{current_tokens[name]} to #{val}. Giving up.")
        all_tokens_set = false
        next
      elsif symbolic_change_map[name]
        symbolic_change_map[name][:tries] += 1
      else
        symbolic_change_map[name] = {
          :current => current_tokens[name],
          :desired => val,
          :tries => 1
        }
      end
      need_reboot = true
      bash "BIOS: change #{name} from #{current_tokens[name]} to #{val}" do
        code "#{pgmname} setting set #{name} #{val}"
      end
    end
    # Test to see if all the raw tokens we want are set.
    # If any are not set, set them and flag that we need a reboot.
    raw_tokens.each do |tok|
      next if %x{ "${pgmname}" test "#{tok}" }.strip.chomp == "set"
      if raw_change_map[tok] && (raw_change_map[tok] > 2)
        Chef::Log.error("Tried #{raw_change_map[tok]} times to set #{tok}. Giving up.")
        all_tokens_set = false
        next
      elsif raw_change_map[tok]
        raw_change_map[tok] += 1
      else
        raw_change_map[tok] = 1
      end
      need_reboot = true
      bash "BIOS: set raw D4 token #{tok}" do
        code "#{pgmname} set #{tok}"
      end
    end
    node[:rebar_wall][:bios][:pec_symbolic_change_map] = symbolic_change_map
    node[:rebar_wall][:bios][:pec_raw_change_map] = raw_change_map
    if need_reboot
      IO.popen("#{pgmname} list_tokens",'r') do |f|
        unless node[:rebar_wall][:bios][:initial_raw_tokens]
          node[:rebar_wall][:bios][:initial_raw_tokens] = f.readlines
        else
          node[:rebar_wall][:bios][:current_raw_tokens] ||= []
          node[:rebar_wall][:bios][:current_raw_tokens] << f.readlines
        end
      end
      bash "Reboot to apply BIOS settings" do
        code "reboot && sleep 120"
      end
    elsif all_tokens_set
      Chef::Log.info("BIOS: All bios settings as expected")
    else
      Chef::Log.error("BIOS: Unable to update all BIOS settings after multiple retries.")
      Chef::Log.error("BIOS: Will continue with install.")
    end
    node.save
  when "new_pec"
    values.each { | name, set_value|
      log("setting #{name} to #{set_value}")
      bash "bios-update-#{name}" do
	code <<-EOH
	   #{pgmname} setting set #{name} #{set_value}
	 EOH
      end
    }
  when "legacy"
    bios_tokens "before changes" do
      action :dump
      pgm pgmname
    end if debug

    values.each { | name, set_value|
      d4_token = set_value[0]
      bash "bios-update-#{name}-#{d4_token}-#{name}" do
	code <<-EOH
	   echo #{pgmname} set #{d4_token}
	   #{pgmname} set #{d4_token}
	 EOH
      end
    }

    bios_tokens "after changes" do
      action :dump
      pgm pgmname
    end if debug
  end
end
node.save
