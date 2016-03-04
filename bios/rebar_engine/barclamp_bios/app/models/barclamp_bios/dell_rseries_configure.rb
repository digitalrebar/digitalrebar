#!/usr/bin/ruby
# Copyright (c) 2014 Victor Lowther
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
# a Dell R series box.


class BarclampBios::DellRseriesConfigure < Role

  # Configure the BIOS settings we care about for Dell R-series gear.
  # @param nr [NodeRole] The noderole that we are acting on behalf of.
  # @param data [Hash] The merged attrib data that we should use for configuration.
  def do_transition(nr,data)
    driver = BarclampBios::Dellrseries.new(nr.node, nr)
    cfg_target = Attrib.get('bios-configuration',data)
    configs = Attrib.get('bios-config-sets',data)
    applied_configs = []
    final_config = {}
    cfg_target.each do |cfg_name|
      get_config_or_panic(cfg_name, configs, applied_configs, final_config, nr)
    end
    # Throw out irrelavent settings or settings that do not need to change.
    final_config.select! do |k,v|
      driver.settings.has_key?(k) && driver.settings[k].current_value != v
    end
    # If we threw away everything, we have nothing to do.
    if final_config.empty?
      update_log(nr, "BIOS already configured for #{cfg_target}")
      return true
    end
    # Propose our new settings
    final_config.each do |k,v|
      update_log(nr, "Setting BIOS setting #{k} to #{v}")
      driver[k] = v
    end
    update_log(nr, "Committing changes")
    if driver.commit
      update_log(nr, "Rebooting node to let changes take effect.")
      if nr.node.bootenv == "local"
        nr.node.power.reboot
      else
        nr.node.power[:reset] ? nr.node.power.reset : nr.node.power.reboot
      end
      # The node is rebooting, so mark it as not alive.
      nr.node.alive = false
      nr.node.save!
    end
  end

  private

  def update_log(nr, string)
    nr.runlog += string + "\n"
    nr.save
  end

  def get_config_or_panic(name, configs, applied_configs, final_config, nr)
    return if applied_configs.include?(name)
    applied_configs << name
    config = configs.find{|e|e["name"] == name}
    raise "Cannot find BIOS config #{name}" unless config
    get_config_or_panic(config["parent"], configs, applied_configs, final_config, nr) if config["parent"]
    update_log(nr, "Adding settings from bios config #{name}")
    final_config.deep_merge!(config["settings"])
  end


end
