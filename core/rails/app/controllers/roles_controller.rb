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
  self.model = Role
  self.cap_base = "ROLE"

  # For now, everyone can see all the roles.
  # This may change iff we tenant them.
  def index
    @list = if params.include? :deployment_id
              find_key_cap(Deployment, params[:deployment_id], cap("READ","DEPLOYMENT")).roles
            elsif params.include? :node_id
              find_key_cap(Node,params[:node_id],cap("READ","NODE")).roles
            else
              Role.all
            end.visible(cap("READ"),@current_user.id)
    respond_to do |format|
      format.html { }
      format.json { render api_index model, @list }
    end
  end

  def graph
    opts = {}
    if params.key? :barclamp_id
      barclamp = find_key_cap(Barclamp, params[:barclamp_id],cap("READ"))
      opts[:barcamp_id] = barclamp.id
    end
    if params.key? :role_id
      role = find_key_cap(Role, params[:role_id],cap("READ"))
      opts[:role_id] = role.id
    end

    roles = visible(model, "READ").where(opts)

    s =  "digraph {"
    roles.each do |r|
      s += "    #{r.id}[label=\"#{r.name}\"];"
    end
    roles.each do |r|
      s += "    #{r.id} -> { #{r.role_requires_children.map{|x| x.role_id}.join(" ")} };" unless r.role_requires_children.empty?
      r.role_preceeds_children.map{|x| x.role_id}.each do |x|
        s += "  #{x} -> { #{r.id} }[color=\"red\"];"
      end
    end
    s += "}"

    render :json => { "string" => s }, :status => 200
  end

  def show
    @role = find_key_cap(model, params[:id],cap("READ"))
    respond_to do |format|
      format.html {  }
      format.json { render api_show @role, "role" }
    end
  end

  def create
    if params.include? :deployment_id
      model.transaction do
        # Arguably, this should be UPDATE since deployment_roles are
        # tightly integrated with deployments.
        @deployment = find_key_cap(Deployment, params[:deployment_id], cap("UPDATE","DEPLOYMENT"))
        role = find_key_cap(model, params[:deployment][:role_id],cap("READ"))
        role.add_to_deployment @deployment
      end
      respond_to do |format|
        format.html { redirect_to deployment_path(@deployment.id) }
        format.json { render api_show @deployment }
      end
    else
      render api_not_supported("post",Role)
    end
  end

  def update
    Role.transaction do
      @role = find_key_cap(model,params[:id],cap("UPDATE")).lock!
      simple_update(@role, %w{description,template})
    end
    respond_to do |format|
      format.html { render :action=>:show }
      format.json { render api_show @role }
    end
  end

  def destroy
    model.transaction do
      @role = find_key_cap(model,params[:role_id],cap("DESTROY"))
      @role.destroy
    end
    render api_delete @role
  end

end
