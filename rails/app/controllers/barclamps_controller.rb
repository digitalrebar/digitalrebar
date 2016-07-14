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

class BarclampsController < ApplicationController

  self.help_contents = Array.new(superclass.help_contents)

  def sample
    render api_sample(Barclamp)
  end

  def match
    attrs = Barclamp.attribute_names.map{|a|a.to_sym}
    objs = Barclamp.where(params.permit(attrs))
    respond_to do |format|
      format.html {}
      format.json { render api_index Barclamp, objs }
    end
  end
  
  def index
    @list = Barclamp.all
    respond_to do |format|
      format.html { }
      format.json { render api_index Barclamp, @list }
    end
  end

  def upload
    @barclamp = Barclamp.find_key params[:barclamp_id] rescue nil
  end

  def show
    @barclamp = Barclamp.find_key params[:id]
    respond_to do |format|
      format.html {  }
      format.json { render api_show @barclamp }
    end
  end

  def update
    params.require(:value)
    if request.patch?
      raise "PATCH update for barclamps not implemented!"
    end
    validate_update(@current_user.current_tenant_id, "BARCLAMP", Barclamp, params[:value])
    @barclamp = Barclamp.import_or_update(params[:value], @current_user.current_tenant_id)
    respond_to do |format|
      format.html {  }
      format.json { render api_show @barclamp }
    end
  end

  def destroy
    render api_not_supported 'delete', 'barclamp'
  end

  def create
    params.require(:value)
    validate_create(@current_user.current_tenant_id, "BARCLAMP", Barclamp)
    @barclamp = Barclamp.import_or_update(params[:value], @current_user.current_tenant_id)
    respond_to do |format|
      format.html {  }
      format.json { render api_show @barclamp }
    end
  end

  # allow barclamp/workload to provide a wizard
  # GREG: TODO: Fix permission and tenancy
  def wizard
    @bc = Barclamp.find_key params[:barclamp_id]

    wiz_name = params[:deployment]
    throw "Deployment Name is required" unless wiz_name

    d = Deployment.find_or_create_by!(name: wiz_name, parent: Deployment.system)

    # track nodes
    nodes = {}
    roles = {}
    @bc.roles.each { |r| roles[r.id] = [] if r.milestone }  # get the available milestones

    # milestone for OS assignment
    cin = Role.find_key 'rebar-installed-node'
    roles[cin] = []

    # find nodes and roles
    params.each do |key, value|
      if key =~ /^role_(\d*)_node_(\d*)$/
        rid = $1.to_i
        nid = $2.to_i
        roles[rid] << nid  ## add nodes that we are going to add
        nodes[nid] ||= (params["wizard"] ? params["wizard"]["node_#{nid}_os"] : "noos") 
      end
    end

    # set the roles in the deployment
    @roles = @bc.roles.sort_by { |r| r.cohort }
    @roles.each do |r|
      r.add_to_deployment d
    end

    # put the nodes into the deployment (need to be done first)
    Node.transaction do
      nodes.each do |nid, os|
        n = Node.find nid
        n.deployment = d
        n.save!
        # and set operating systems for selected nodes (unless docker)
        Rails.logger.debug "Barclamp wizard: set #{n.name} into deployment #{d.name}"
        if n.is_docker_node?
          Rails.logger.info "Barclamp wizard: NOT assigning #{os} to node #{n.name} because it's DOCKER"
        elsif os == "noos"
          Rails.logger.info "Barclamp wizard: NO os assignment for node #{n.name} in deployment #{d.name}"
        else
          Attrib.set "provisioner-target_os", n, os, :user if os
          Rails.logger.info "Barclamp wizard: assigning #{os} to node #{n.name} in deployment #{d.name}"
        end
      end
    end

    # we need to do this in cohort order!
    roles = roles.sort_by { |r, n| Role.find(r).cohort }

    # set the roles on each node
    roles.each do |rid, nodes|
      r = Role.find rid
      Rails.logger.info "Barclamp wizard: deployment-role #{r.name} added to deployment #{d.name}"
      nodes.each do |nid|
        n = Node.find nid
        r.add_to_node_in_deployment(n, d) unless n.is_docker_node? and r.id == cin
        Rails.logger.info "Barclamp wizard: #{r.name} adding node #{n.name} in deployment #{d.name}"
      end
      Rails.logger.debug "Barclamp wizard: made all changes for role #{r.name}"
    end

    Rails.logger.debug "Barclamp wizard: opening deployment #{deployment_path(:id=>d.id)}"
    redirect_to deployment_path(:id=>d.id)
  

  end

end

