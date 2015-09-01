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
      d = Deployment.find_or_create_by_name! :name=>wiz_name, :parent=>Deployment.system

      # milestone for OS assignment
      wiz = Role.find_key 'crowbar-installed-node'
      #wiz.add_to_deployment d

      params.each do |key, value|
        if key =~ /^node_([0-9]*)_role_([0-9]*)$/
          n = Node.find $1.to_i
          r = Role.find $2.to_i
          Rails.logger.info "Barclamp wizard #{r.name} added node #{n.name}"
        else
          Rails.logger.info "Barclamp wizard MISS #{key} #{value} #{$0} #{$1}"
        end
      end
      #redirect_to deployment_path(:id=>d.id)
      redirect_to barclamp_path(:id=>@bc.id)
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

