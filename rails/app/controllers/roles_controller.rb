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
class RolesController < ApplicationController

  def index
    @list = if params.include? :deployment_id
              Deployment.find_key(params[:deployment_id]).roles
            elsif params.include? :node_id
              Node.find_key(params[:node_id]).roles
            else
              Role.all
            end
    respond_to do |format|
      format.html { }
      format.json { render api_index Role, @list }
    end
  end

  def show
    @role = Role.find_key params[:id]
    respond_to do |format|
      format.html {  }
      format.json { render api_show @role, "role" }
    end
  end

  def create
    if params.include? :deployment_id
      @deployment = Deployment.find_key params[:deployment_id]
      role = Role.find_key params[:deployment][:role_id].to_i 
      role.add_to_deployment @deployment
      respond_to do |format|
        format.html { redirect_to deployment_path(@deployment.id) }
        format.json { render api_show @deployment }
      end
    else
      render api_not_supported("post",Role)
    end
  end

  def update
    @role = Role.find_key params[:id]
    @role.update_attributes!(params.permit(:description))
    if params.key? :template
      @role.template = params[:template]
      @role.save!
    end
    respond_to do |format|
      format.html { render :action=>:show }
      format.json { render api_show @role }
    end
  end

  def destroy
    @role = Role.find_key params[:role_id]
    @role.destroy
    render api_delete @role
  end

end
