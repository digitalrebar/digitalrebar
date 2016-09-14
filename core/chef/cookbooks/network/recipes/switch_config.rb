# Copyright 2012, Dell
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

def get_switch_config(switch_config, switch_name)
  a_switch_config = switch_config[switch_name]

  if a_switch_config.nil?
    a_switch_config = {}
    switch_config[switch_name] = a_switch_config
    a_switch_config["unique_vlans"] = {}
    a_switch_config["interfaces"] = {}
    a_switch_config["lags"] = {}
    a_switch_config["next_lag_id"] = 1
  end

  a_switch_config
end


def setup_interface(switch_config, a_node, conduit, switch_name, interface_id )

    a_switch_config = get_switch_config(switch_config, switch_name)
    unique_vlans = a_switch_config["unique_vlans"]
    interfaces = a_switch_config["interfaces"]

    # Find the conduit associated with the interface
    a_node["rebar"]["network"].each do |network_name, network|
      next if network["conduit"] != conduit
      vlan = network["vlan"]
      unique_vlans[vlan] = network_name

      interfaces[interface_id] = {} if interfaces[interface_id].nil?
      vlans_for_interface = interfaces[interface_id]
      vlans_for_interface[vlan] = network["use_vlan"]
    end
end

admin_ip = node.address.addr

switch_config={}

search(:node, "*:*").each do |a_node|
  node_map = Chef::Recipe::Barclamp::Inventory.build_node_map(a_node)
  node_map.each do |conduit, conduit_info|
    if_list = conduit_info["if_list"]
    team_mode = conduit_info["team_mode"] rescue nil

    # Figure out the config for all switches attached to this one conduit for this one node
    switch_ports = {}
    if_list.each do |intf|
      sw=a_node["rebar_ohai"]["switch_config"][intf] rescue {}
      next if sw.nil?
      next unless sw["switch_name"] && sw["switch_port"] && sw["switch_port_name"]
      next if sw["switch_port"] == -1
      switch_name=sw["switch_name"]
      switch_port=sw["switch_port"]
      switch_port_name=sw["switch_port_name"]
      switch_ports[switch_name] ||= []
      switch_ports[switch_name] << switch_port_name
    end

    switch_ports.each do |switch_name, ports|
      ports.sort!

      interface_ids = []
      if ports.size > 1 && team_mode != 5 && team_mode != 6

        a_switch_config = get_switch_config(switch_config, switch_name)
        lag_id = a_switch_config["next_lag_id"]
        a_switch_config["next_lag_id"] += 1

        lag = {}
        lag["lag_id"] = lag_id
        lag["ports"] = ports

        a_switch_config["lags"][lag_id] = lag

        interface_ids << lag_id.to_s
      else
        # If we're here then there is either only 1 port connected to this switch or
        # there are multiple ports in team mode 5 or 6, so treat as interfaces
        interface_ids = ports
      end

      interface_ids.each do |interface_id|
        setup_interface(switch_config, a_node, conduit, switch_name, interface_id )
      end
    end
  end
end

directory "/opt/dell/switch" do
  mode 0755
  owner "root"
  group "root"
end

switch_config.each do |switch_name, a_switch_config|

  # For testing on a virtual admin node
  switch_name = "virtual" if switch_name == -1

  switch_config_file_name = "/opt/dell/switch/#{switch_name.gsub( /:/, '_')}_switch_config.json"

  template switch_config_file_name do
    mode 0644
    owner "root"
    group "root"
    source "switch_config.erb"
    variables(
      :admin_node_ip => admin_ip,
      :unique_vlans => a_switch_config["unique_vlans"],
      :lags => a_switch_config["lags"],
      :interfaces => a_switch_config["interfaces"]
    )
  end
end
