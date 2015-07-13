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
    @inventory = {  ansible_ssh_user: "root",  ansible_ssh_port: 22 }

    # list of deployment groups
    ds = params[:id] ? [Deployments.find_key(params[:id])] : Deployments.all
    ds do |d|
      hosts = d.addressable_nodes.map { |n| n.name }
      children = d.children.map { |c| c.name }
      vars = {}
      vars['networks'] = d.networks.map { |n| n.name }
      vars['roles'] = d.deployment_roles.map { |dr| dr.name }
      @inventory[d.name] = { hosts: hosts, children: children, vars: vars }
    end

    # list of hosts with variable information
    hostvars = {}
    ns = params[:id] ? ds.first.nodes : Nodes.all
    ns do |n|
      hostvars[n.name] = {}
      hostvars[n.name]["alive"] = n.alive
      hostvars[n.name]["available"] = n.available
      hostvars[n.name]["state"] = n.state
      hostvars[n.name]["os"] = n.get_attrib("os") || 'unknown'
      hostvars[n.name]["cpus"] = n.get_attrib("cpus") || -1
      hostvars[n.name]["disks"] = n.get_attrib("number_of_disks") || -1
      hostvars[n.name]["ram"] = n.get_attrib("ram") || -1
      n.network_allocations.each do |na|
        hostvars[n.name][na.name] = na.address
      end
      # provide interfaces here
    end 
    @inventory["_meta"] = { hostvars: hostvars }

    respond_to do |format|
      format.json { @inventory }
    end
  end

end
