# Copyright 2013, Dell
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
class DashboardController < ApplicationController

  def layercake
    # we may want to move this into the database at some point
    taxmap = JSON::load File.open(File.join("config", "layercake.json"), 'r')
    @layers = {}
    @status = {}
    taxmap["layers"].each { |k| @layers[k]=[] }
    NodeRole.all.each do |nr|
      layer = taxmap[nr.role.name] || 'apps'
      @layers[layer] << nr 
      #if nr.state == NodeRole::ERROR
      #  @status[layer] = 'alert'
      #elsif (nr.state == NodeRole::PROPOSED)
      #  @status[layer] = 'user'
      #elsif nr.state != NodeRole::ACTIVE
      #  @status[layer] = 'system'
      #end
    end

    respond_to do |format|
      format.html { }
    end
  end    

  def group_change
    # TODO: not used?
    node = Node.find_by_name params[:id]
    if node.nil?
      raise "Node #{params[:id]} not found.  Cannot change group" 
    else
      group = params[:group]
      if params.key? 'automatic'
        node.group=""
      else
        node.group=group
      end
      node.save
      Rails.logger.info "node #{node.name} (#{node.alias}) changed its group to be #{node.group.empty? ? 'automatic' : group}."
      render :inline => "SUCCESS: added #{node.name} to #{group}.", :cache => false 
    end
  end
  

  # Bulk Edit
  def list
    if request.put?
      nodes = {}
      params.each do |k, v|
        if k.starts_with? "node:"
          parts = k.split ':'
          node = parts[1]
          area = parts[2]
          nodes[node] ||= {} 
          nodes[node][area] = (v.empty? ? nil : v)
        end
      end
      succeeded = []
      failed = []
      nodes.each do |node_name, values|
        node = Node.find_key node_name
        begin
          node.update_attributes! values
          succeeded << node.alias
        rescue Exception=>e
          Rails.logger.info "user attempted dashboard.list put for node #{node.name} raised error #{e.message}"
          failed << node.alias
        end
      end
      if failed.length>0
        flash[:notice] = I18n.t('failed', :scope=>'dashboard.list', :list=>failed.join(','))
      elsif succeeded.length>0
        flash[:notice] = I18n.t('updated', :scope=>'dashboard.list', :list=>succeeded.join(','))
      end
    end
    @nodes = if params.key? :deployment
      @deployment = Deployment.find_key params[:deployment]
      @deployment.nodes
    else
      Node.all
    end
  end

  def families
    @families = {}
    Node.all.each do |n|
      f = n.family.to_s  
      @families[f] = {:names=>[], :family=>n.family} unless @families.has_key? f
      @families[f][:names] << {:alias=>n.alias, :description=>n.description, :handle=>n.name}
    end
  end

  # multi-step node allocation
  def getready
    if request.get?
      @nodes = Deployment.system.nodes
    elsif request.post?
      d = Deployment.find_or_create_by_name! :name=>params[:deployment], :parent=>Deployment.system
      if params[:conduit]
        n = Network.find_or_create_by_name! :name=>params[:deployment], :conduit=>params[:conduit], :deployment=>d, :v6prefix=>Network::V6AUTO
        NetworkRange.create! :name=>params[:range], :network=>n, :first=>params[:first_ip], :last=>params[:last_ip] if n.ranges.count < 2
      end

      # milestone for OS assignment
      ready = Role.find_key 'crowbar-installed-node'
      ready.add_to_deployment d
      pilot_network = Role.find_key 'network-pilot'
      pilot_network.add_to_deployment d

      params.each do |node_id, value|
        if node_id =~ /^node_([0-9]*)/
          n = Node.find $1.to_i
          Node.transaction do
            n.deployment = d
            n.save!
            Rails.logger.info "Dashboard GetReady Deployment #{d.name} added node #{n.name}"
            # assign milestone for OS assignment
            ready.add_to_node_in_deployment n, d unless n.is_docker_node?
            pilot_network.add_to_node_in_deployment n, d
          end
          # set desired OS to attribute
          Attrib.set "provisioner-target_os", n, params["dashboard"]["#{node_id}_os"], :user unless n.is_docker_node?
        end
      end
      redirect_to deployment_path(:id=>d.id)
    end
  end

end
