# Copyright 2015, RackN
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
#
class InventoryController < ApplicationController

  # API GET /api/v2/inventory
  # uses the Ansible dynamic inventory format
  def index

    @inventory = {}
    hostvars = {}

    if !params[:hostvar] || params[:hostvar] == "none"

      available_os = Attrib.get("provisioner-available-oses", Node.admin.where(:available => true).first) rescue []
      @inventory[:all] = { vars: { ansible_ssh_user: "root",  ansible_ssh_port: 22, available_os: available_os.keys }}

      # list of deployment groups
      d = Deployment.find_key params[:id] || 'system'
      ds = (d.name != 'system' ? [d] : Deployment.all)
      ds.each do |d|
        hosts = d.nodes.map{ |n| n.name if !n.admin and !n.system  }.delete_if{|i| !i}
        children = Deployment.children_of(d).map { |c| c.name }
        vars = {}
        vars['networks'] = d.networks.map { |n| n.name }
        vars['roles'] = d.deployment_roles.map { |dr| dr.name }
        @inventory[d.name] = { hosts: hosts, vars: vars }
        @inventory[d.name][:children] = children if children.length > 0
      end
    end

    ns = if params[:hostvar] == 'none'
      [] # do nothing
    elsif params[:hostvar]
      ns = [Node.find_key(params[:hostvar])]
    else
      ns = params[:id] ? ds.first.nodes : Node.all
    end

    # groups
    Group.all.each do |g|
      @inventory["#{g.category}_#{g.name}"] = []
      g.nodes.each do |n|
        @inventory["#{g.category}_#{g.name}"] << n.name
      end
    end


    ns.each do |n|
      next if n.admin or n.system
      hostvars[n.name] = {}
      hostvars[n.name]["alive"] = n.alive
      hostvars[n.name]["available"] = n.available
      hostvars[n.name]["state"] = NodeRole::STATES[n.state]
      hostvars[n.name]["os"] = n.get_attrib("os") || 'unknown'
      attribs = {"cpu_count" => -1, "memory" => 1, "number_of_drives" => -1}
      attribs.each { |k,v| hostvars[n.name][k] = n.get_attrib(k) || v }
      n.network_allocations.each do |na|
        hostvars[n.name]["network.#{na.network.name}.#{na.network_range.name}"] = na.address.to_s
      end
      nets = n.get_attrib("network-current_config")["nets"] rescue []
      nets.each do |net| 
        hostvars[n.name]["interface.#{net[0]}"] = net[3] rescue ""
      end
    end 
    if params[:hostvar] and params[:hostvar] != 'none'
      @inventory = hostvars[ns.first.name]
    else
      @inventory["_meta"] = { hostvars: hostvars } if hostvars.length > 0
    end

    render json: @inventory, content_type: "application/vnd.rebar.inventory+json; version=2.0"
  end

end
