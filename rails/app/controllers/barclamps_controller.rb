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
      @roles = @bc.roles.keep_if { |r| r.milestone }
      @nodes = Deployment.system.nodes.where(:admin=>false, :system=>false)
    elsif request.post?

      wiz_name = params[:deployment]
      throw "Deployment Name is required" unless wiz_name

      Deployment.transaction do

        d = Deployment.find_or_create_by_name! :name=>wiz_name, :parent=>Deployment.system

        # track nodes
        nodes = {}
        roles = {}
        @bc.roles.each { |r| roles[r.id] = [] if r.milestone }  # get the available milestones

        # milestone for OS assignment
        cin = Role.find_key 'crowbar-installed-node'
        roles[cin] = []

        # find nodes and roles
        params.each do |key, value|
          if key =~ /^node_([0-9]*)_role_([0-9]*)$/
            nid = $1.to_i
            rid = $2.to_i
            nodes[nid] = params["wizard"]["node_#{nid}_os"]   # we only want to do the node stuff once
            roles[rid] << nid  ## add nodes that we are going to add
          end
        end

        # set the roles
        roles.each do |rid, nodes|
          r = Role.find rid
          r.add_to_deployment d
          nodes.each do |nid|
            n = Node.find nid
            r.add_to_node_in_deployment(n, d) unless n.is_docker_node? and r.id == cin
            Rails.logger.info "Barclamp wizard: #{r.name} adding node #{n.name} in deployment #{d.name}"
          end
        end

        # set operating systems for selected nodes (unless docker)
        nodes.each do |nid, os|
          n = Node.find nid
          n.deployment = d
          n.save!
          next if n.is_docker_node?
          Attrib.set "provisioner-target_os", n, os, :user
          Rails.logger.info "Barclamp wizard: assigning #{os} to node #{n.name} in deployment #{d.name}"
        end
      
        redirect_to deployment_path(:id=>d.id)

      end

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

