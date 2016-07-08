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
    NodeRole.all.joins(:node,:role).select("node_roles.*,
                                      roles.name as role_name,
                                      nodes.name as node_name,
                                      nodes.alive as node_alive,
                                      nodes.available as node_available").each do |nr|
      layer = taxmap[nr.role_name] || 'apps'
      @layers[layer] << nr 
      if nr.state == NodeRole::ERROR
        @status[layer] = 'alert'
      elsif (nr.state == NodeRole::PROPOSED)
        @status[layer] = 'user' unless @status[layer] == 'alert'
      elsif nr.state != NodeRole::ACTIVE
        @status[layer] = 'system' unless @status[layer] == 'alert'
      end
    end

    respond_to do |format|
      format.html { }
      format.json { render :json => @status.to_json } # also respond to json for layercake ajax updating
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
      Rails.logger.info "node #{node.name} changed its group to be #{node.group.empty? ? 'automatic' : group}."
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
          nodes[node][area] = v
        end
      end
      succeeded = []
      failed = []
      nodes.each do |node_name, values|
        node = Node.find_key node_name
        begin
          node.update_attributes! values
          succeeded << node.name
        rescue StandardError=>e
          Rails.logger.info "user attempted dashboard.list put for node #{node.name} raised error #{e.message}"
          failed << node.name
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
      @deployment.nodes.where(:system=>false)
    else
      Node.all.where(:system=>false)
    end
  end

  # group nodes into categories
  def families

    # the attribs list is passed as the ID w/ pipe delimiters
    @families = if params[:id]
      params[:id].split "|"
    else
      ['cpu_count', 'memory',  'number_of_drives']
    end
    @nodes = {}
    # this works by building a single key with all the requested attributes in order
    # that makes it easy to sort and collect like attributes
    Node.all.each do |n|
      next if n.system
      key = @families.map { |f| (n.get_attrib(f) || t("unknown")).to_s }
      @nodes[key] ||= {}
      @nodes[key][n.id] = n.name
    end
  end

  # multi-step node allocation
  def getready
    if request.get?
      @nodes = Deployment.system.nodes.where(:admin=>false, :system=>false)
    elsif request.post?

      ready_name = params[:deployment]
      throw "Deployment Name is required" unless ready_name
      d = Deployment.find_or_create_by!(name: ready_name, parent: Deployment.system)
      throw "Did not create Deployment" unless d
      network_name = ready_name + "-default"
      n = Network.find_key(network_name) rescue nil
      if !n and params[:conduit]
        n = Network.find_or_create_by!(name: network_name, category: ready_name, group: "default", conduit: params[:conduit], deployment: d, v6prefix: Network::V6AUTO)
        begin
          NetworkRange.create! :name=>params[:range], :network=>n, :first=>params[:first_ip], :last=>params[:last_ip] if n.ranges.count < 2
        rescue
          Rails.logger.warn "Dashboard GetReady did not create Network #{n.name} Range #{params[:range]}.  Likely conflicted with existing IP range."
        end
      end

      # milestone for OS assignment
      ready = Role.find_key 'rebar-installed-node'
      ready.add_to_deployment d
      ready_network = Role.find_key "network-#{network_name}"
      ready_network.add_to_deployment d

      params.keys.each do |node_id|
        Rails.logger.debug "Dashboard GetReady Checking #{node_id}"
        if node_id =~ /^node_([0-9]*)/
          n = Node.find $1.to_i
          Rails.logger.debug "Dashboard GetReady using Node #{n.inspect}"
          Node.transaction do
            n.deployment = d
            n.save!
          end
          # assign milestone for OS assignment
          Rails.logger.info "Dashboard GetReady Deployment #{d.name} added node #{n.name}"
          ready.add_to_node_in_deployment n, d unless n.is_docker_node?
          nics = n.attrib_nics.count rescue 0
          ready_network.add_to_node_in_deployment n, d if nics > 0
          # set desired OS to attribute
          Attrib.set "provisioner-target_os", n, params["dashboard"]["#{node_id}_os"], :user unless n.is_docker_node?
        end
      end
      redirect_to deployment_path(:id=>d.id)
    end
  end

  def wizard
    #@bc = Barclamp.find_key params[:barclamp_id]

    name = params[:name]
    metal_os = params[:os]
    provider = params[:provider]

    provider_id = Provider.find_key(provider[:name]).id if provider
    provider_os = provider[:os]
    provider_id ||= Provider.find_by!(name: 'metal').id

    nodes = params[:nodes]
    metalNodes = nodes.select{|i| i[:node_id] > 0}
    cloudNodes = nodes - metalNodes

    roles = []
    nodes.each do |n|
      roles += n[:roles] || []
    end
    roles.uniq!


    # milestone for OS assignment
    cin = Role.find_key 'rebar-installed-node'
    roles << cin

    # use ids for each role
    roles.map! { |r| Role.find(r).id }

    node_roles = {} # map[role_id][]node_id
    roles.each { |r| node_roles[r] = [] }

    throw "Deployment Name is required" unless name

    d = Deployment.find_or_create_by!(name: name, parent: Deployment.system)

    # set the roles in the deployment
    roles = roles.sort_by { |r| r.cohort }
    roles.each do |r|
      r.add_to_deployment d
    end


    # put the nodes into the deployment (need to be done first)
    Node.transaction do
      metalNodes.each do |n|
        node = Node.find n[:node_id]
        os = n[:os]
        node.deployment = d
        node.save!

        n[:roles].each do |r|
          node_roles[r] << node.id
        end
        # and set operating systems for selected nodes (unless docker)
        Rails.logger.debug "Wizard: set #{n.name} into deployment #{d.name}"
        if n.is_docker_node?
          Rails.logger.info "Wizard: NOT assigning #{os} to node #{n.name} because it's DOCKER"
        elsif os == "noos"
          Rails.logger.info "Wizard: NO os assignment for node #{n.name} in deployment #{d.name}"
        else
          Attrib.set "provisioner-target_os", n, os, :user if os
          Rails.logger.info "Wizard: assigning #{os} to node #{n.name} in deployment #{d.name}"
        end
      end

      nodeIndex = 0
      cloudNodes.each do |n|
        n[:nodes] = []
        hints = n[:hints] || {
          'use-proxy' => false,
          'use-ntp' => false,
          'use-dns' => false,
          'use-logging' => false,
          'provider-create-hint': {
            'hostname': name + "-" + nodeIndex.to_s
          }
        }

        if n[:provider]
          node_provider = n[:provider][:name]
          node_provider_id = provider.find_key(n[:provider][:name]).id
          node_provider_os = n[:provider][:os] || provider_os
        else
          node_provider = provider[:name]
          node_provider_id = provider_id
          node_provider_os = provider_os
        end

        (n[:node_count] || 1).times do |i|

          node = Node.create!({
            name: name + "-" + nodeIndex.to_s + "." + node_provider + ".neode.org",
            description: "created by rebar",
            deployment_id: d.id,
            provider_id: node_provider_id,
            system: false,
          })
          # Set any hints we got with node creation.
          hints.each do |k, v|
            Attrib.set(k, node, v)
          end

          n[:nodes] << node

          n[:roles].each do |r|
            node_roles[r] << node.id
          end
          nodeIndex += 1
        end
      end
    end

    # set the roles on each node
    node_roles.each do |rid, nodes|
      r = Role.find rid
      Rails.logger.info "Wizard: deployment-role #{r.name} added to deployment #{d.name}"
      nodes.each do |nid|
        n = Node.find nid
        r.add_to_node_in_deployment(n, d) unless n.is_docker_node? and r.id == cin
        Rails.logger.info "Wizard: #{r.name} adding node #{n.name} in deployment #{d.name}"
      end
      Rails.logger.debug "Wizard: made all changes for role #{r.name}"
    end

    Rails.logger.debug "Wizard: opening deployment #{deployment_path(:id=>d.id)}"
    redirect_to deployment_path(:id=>d.id)
  

  end

end
