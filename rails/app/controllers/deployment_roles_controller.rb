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

class DeploymentRolesController < ApplicationController
  self.model = DeploymentRole
  self.cap_base = "DEPLOYMENT"
 
  def index
    @list = if params.has_key? :deployment_id
              find_key_cap(Deployment,params[:deployment_id], cap("READ")).deployment_roles
            elsif params.has_key? :role_id
              Role.find_key(params[:role_id]).deployment_roles.visible(cap("READ"),@current_user.id)
            else
              visible(model,cap("READ"))
            end
    respond_to do |format|
      format.html { }
      format.json { render api_index model, @list }
    end
  end

  def show
    # allow lookup by name
    model.transaction do
      if params.has_key? :deployment
        deployment = find_key_cap(Deployment, params[:deployment], cap("READ"))
        role = Role.find_key params[:id]
        @deployment_role = model.find_by!(deployment_id: deployment.id, role_id: role.id)
      else
        @deployment_role = find_key_cap(model, params[:id], cap("READ"))
      end
    end
    respond_to do |format|
      format.html {  }
      format.json { render api_show @deployment_role }
    end
  end

  def create
    DeploymentRole.transaction do
      # When we start caring about ROLE_READ this will need updating.
      r = Role.find_key(params[:role] || params[:role_id] || params[:add_role][:role_id])
      # Creating a deployment_role is effectively updating the
      # deployment it is a part of, hence the UPDATE instead of CREATE
      # here.  If deployment_roles get their own capability namespace,
      # this should change.
      d = find_key_cap(Deployment,
                       params[:deployment] ||
                       params[:deployment_id],
                       cap("UPDATE"))
      params[:role_id] = r.id
      params[:deployment_id] = d.id
      params.require(:role_id)
      params.require(:deployment_id)
      @deployment_role = DeploymentRole.create!(params.permit(:data, :role_id, :deployment_id))
    end
    respond_to do |format|
      format.html { redirect_to deployment_path(params[:deployment_id]) }
      format.json { render api_show @deployment_role }
    end
  end

  def update
    DeploymentRole.transaction do
      @deployment_role = find_key_cap(model, params[:id],cap("UPDATE")).lock!
      if request.patch?
        raise "Cannot PATCH deployment roles!"
      end
      params.require(:data)
      @deployment_role.data = params[:data]
      @deployment_role.save!
    end
    render api_show @deployment_role
  end

  def destroy
    model.transaction do
      @deployment_role = find_key_cap(model,params[:id],cap("DESTROY"))
      @deployment_role.destroy
    end
    respond_to do |format|
      format.html { redirect_to deployment_path(@deployment_role.deployment_id) }
      format.json { render api_delete @deployment_role }
    end

    render api_delete @deployment_role
  end

  def propose
    model.transaction do
      @deployment_role = find_key_cap(model, params[:deployment_role_id],cap("PROPOSE"))
      @deployment_role.propose
    end
    respond_to do |format|
      format.html { redirect_to deployment_role_path(@deployment_role.id) }
      format.json { render api_show @deployment_role }
    end
  end

  def commit
    model.transaction do
      @deployment_role = find_key_cap(model, params[:deployment_role_id],cap("COMMIT"))
      @deployment_role.commit
    end
    respond_to do |format|
      format.html { redirect_to deployment_role_path(@deployment_role.id) }
      format.json { render api_show @deployment_role }
    end
  end

end
