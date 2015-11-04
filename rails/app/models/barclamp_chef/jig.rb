# Copyright 2013, Dell
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
#

require 'json'
require 'chef'
require 'fileutils'
require 'thread'

class BarclampChef::Jig < Jig
  @@load_role_mutex ||= Mutex.new
  @@reload_mutex ||= Mutex.new

  def make_run_list(nr)
    runlist = Array.new
    runlist << "recipe[barclamp]"
    runlist << "recipe[ohai]"
    runlist << "recipe[utils]"
    runlist << "role[#{nr.role.name}]"
    Rails.logger.info("Chefjig: discovered run list: #{runlist}")
    Chef::RunList.new(*runlist)
  end

  def stage_run(nr)
    return {
      :runlist => make_run_list(nr),
      :data => super(nr)
    }
  end

  def run(nr,data)
    prep_chef_auth
    if nr.role.respond_to?(:jig_role)
      jr = nr.role.jig_role(nr)
      unless (Chef::Role.load(jr['name']) rescue nil)
        begin
          Chef::Role.json_create(nr.role.jig_role(nr)).save
        rescue Net::HTTPServerException
          Rails.logger.info("Role #{jr["name"]} already exists in the Chef server")
        end
      end
    end
    unless (Chef::Role.load(nr.role.name) rescue nil)
      raise "Chef role for #{nr.role.name} is not in the Chef server!"
    end
    chef_node, chef_noderole = chef_node_and_role(nr.node)
    chef_noderole.default_attributes(data[:data])
    chef_noderole.run_list(data[:runlist])
    chef_noderole.save
    # For now, be bloody stupid.
    # We should really be much more clever about building
    # and maintaining the run list, but this will do to start off.
    chef_node.attributes.normal = {}
    chef_node.run_list(Chef::RunList.new(chef_noderole.to_s))
    chef_node.save
    # SSH into the node and kick chef-client.
    # If it passes, go to ACTIVE, otherwise ERROR.
    out,err,ok = nr.node.ssh("chef-client")
    raise("Chef jig run for #{nr.name} failed\nOut: #{out}\nErr:#{err}") unless ok.success?
    # Reload the node, find any attrs on it that map to ones this
    # node role cares about, and write them to the wall.
    Rails.logger.info("Chef jig: Reloading Chef objects")
    chef_node, chef_noderole = chef_node_and_role(nr.node)
    NodeRole.transaction do
      wall = mash_to_hash(chef_node.attributes.normal)
      discovery = {"ohai" => mash_to_hash(chef_node.attributes.automatic.to_hash)}
      Rails.logger.debug("Chef jig: Saving runlog")
      nr.update!(runlog: out)
      Rails.logger.debug("Chef jig: Saving wall")
      nr.update!(wall: wall)
      Rails.logger.debug("Chef jig: Saving discovery attributes")
      nr.node.discovery_merge(discovery)
    end
  end

  def create_node(node)
    cb_nodename = node.name
    Rails.logger.info("ChefJig Creating node #{cb_nodename}")
    prep_chef_auth
    cb_noderolename = node_role_name(cb_nodename)
    chef_node = Chef::Node.build(cb_nodename)
    chef_role = Chef::Role.new
    chef_role.name(cb_noderolename)
    chef_client = Chef::ApiClient.new
    chef_client.name(cb_nodename)
    [chef_node.save, chef_role.save, chef_client.save]
  end

  def delete_node(node)
    prep_chef_auth
    name = Attrib.get('chef-client_name', node)
    Rails.logger.info("ChefJig Deleting node #{name}")
    chef_client = (Chef::ApiClient.load(name) rescue nil)
    chef_client.destroy if chef_client
    chef_node_and_role(node).each do |i|
      i.destroy
    end
  end

  private

  def node_role_name(n_name)
    "rebar-#{n_name.tr(".","_")}"
  end

  
  def mash_to_hash(src)
    case
    when src.kind_of?(Hash)
      res = Hash.new
      src.each do |k,v|
        res[k.to_s] = mash_to_hash(v)
      end
      res
    when src.kind_of?(Array)
      res = Array.new
      src.each do |v|
        res << mash_to_hash(v)
      end
      res
    else
      src
    end
  end

  def chef_node_and_role(node)
    @@reload_mutex.synchronize do
      cb_name = Attrib.get('chef-client_name', node)
      Rails.logger.info("ChefJig: Reloading chef node and role info for #{cb_name}")
      prep_chef_auth
      [Chef::Node.load(cb_name),Chef::Role.load(node_role_name(cb_name))]
    end
  end

  def prep_chef_auth
    reload if server.nil? || server.empty?
    Chef::Config[:client_key] = key
    Chef::Config[:chef_server_url] = server
    Chef::Config[:node_name] = client_name
    # Yes, I know this is insecure.  We just have to deal for now.
    Chef::Config[:ssl_verify_mode] = :verify_none
  end

end # class
