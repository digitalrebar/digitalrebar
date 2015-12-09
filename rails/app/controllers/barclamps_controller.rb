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
    @barclamp = Barclamp.import_or_update(params[:value])
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
    @barclamp = Barclamp.import_or_update(params[:value])
    respond_to do |format|
      format.html {  }
      format.json { render api_show @barclamp }
    end
  end

  # allow barclamp/workload to provide a wizard
  def wizard
    @bc = Barclamp.find_key params[:barclamp_id]
    if request.get?

      @roles = @bc.roles.to_a.keep_if{ |r| r.milestone }.sort_by { |r| r.cohort }
      @nodes = Deployment.system.nodes.where(:admin=>false, :system=>false)

      provisioner = Role.find_key 'provisioner-base-images'
      admin = provisioner.nodes.first
      @available_os = Attrib.get("provisioner-available-oses", admin).map{ |k,v| k } rescue []
      @initial_role = Attrib.get('provisioner-target_os', Role.find_by(name: 'provisioner-os-install')) rescue "fred"
      @errors = []

      # look for required roles and add to errors list
      needs = @bc.wizard.inspect["requires"]["roles"] || [] rescue []
      needs.each do |r|  
        unless (Role.find_key r rescue nil)
          @errors << I18n.t('role_missing', :scope => 'barclamps.wizard', :role => r)
        end
      end

    elsif request.post?

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
        if key =~ /^role_([0-9]*)_node_([0-9]*)$/
          rid = $1.to_i
          nid = $2.to_i
          roles[rid] << nid  ## add nodes that we are going to add
          nodes[nid] = params["wizard"]["node_#{nid}_os"] if params["wizard"] # we only want to do the node stuff once
        end
      end

      # we need to do this in cohort order!
      roles = roles.sort_by { |r, n| Role.find(r).cohort }

      # set the roles
      roles.each do |rid, nodes|
        r = Role.find rid
        r.add_to_deployment d
        Rails.logger.info "Barclamp wizard: deployment-role #{r.name} added to deployment #{d.name}"
        nodes.each do |nid|
          n = Node.find nid
          r.add_to_node_in_deployment(n, d) unless n.is_docker_node? and r.id == cin
          Rails.logger.info "Barclamp wizard: #{r.name} adding node #{n.name} in deployment #{d.name}"
        end
        Rails.logger.debug "Barclamp wizard: made all changes for role #{r.name}"
      end

      # set operating systems for selected nodes (unless docker)
      nodes.each do |nid, os|
        n = Node.find nid
        n.deployment = d
        n.save!
        Rails.logger.debug "Barclamp wizard: set #{n.name} into deployment #{d.name}"
        unless n.is_docker_node?
          Attrib.set "provisioner-target_os", n, os, :user if os
          Rails.logger.info "Barclamp wizard: assigning #{os} to node #{n.name} in deployment #{d.name}"
        else
          Rails.logger.info "Barclamp wizard: NOT assigning #{os} to node #{n.name} because it's DOCKER"
        end
      end

      Rails.logger.debug "Barclamp wizard: opening deployment #{deployment_path(:id=>d.id)}"
      redirect_to deployment_path(:id=>d.id)
      
    end

  end

  #
  # Barclamp catalog
  # 
  # Provides restful API call for 
  # List actions       /barclamp:/api_version:/catalog  GET 
  # 
  add_help(:catalog)
  def catalog     
    @bc = barclamp
    render :json => { :name=>"unknown"} unless @bc

    # TODO: find actions by introspection?
    render :json => {
      :name=>@bc.name, 
      :version=>@bc.version, 
      :api_version=>@bc.api_version,
      :api_version_accepts=>@bc.api_version_accepts, 
      :actions=>['node','group','jig', 'attrib'],
      :license=>@bc.license,
      :copyright=>@bc.copyright
    }
  end

end

