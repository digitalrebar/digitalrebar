# Copyright 2013-4, Dell
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
class DeploymentsController < ApplicationController
  self.model = Deployment
  self.cap_base = "DEPLOYMENT"

  def index
    @list = visible(model, cap("READ")).order("id DESC")
    respond_to do |format|
      format.html { }
      format.json { render api_index model, @list }
    end
  end

  def show
    respond_to do |format|
      format.html {
        model.transaction do
          @deployment = find_key_cap(model, params[:id], cap("READ"))
          @roles = @deployment.deployment_roles.sort{|a,b|a.role.cohort <=> b.role.cohort}
          # remove the service roles
          @roles.delete_if { |r| r.role.service }
          # alpha lists by ID
          @nodes = visible(Node,cap("READ","NODE")).
                   where(system: false).
                   order("name ASC").select do |n|
            (n.deployment_id == @deployment.id) ||
              (n.node_roles.where(:deployment_id => @deployment.id).count > 0)
          end
        end
      }
      format.json { render api_show find_key_cap(model, params[:id], cap("READ")) }
    end
  end

  def create
    Deployment.transaction do
      permits = [:name,:parent_id,:description,:tenant_id]
      if params[:system]
        raise "Only one system deployment permitted" if Deployment.exists?(system: true)
        permits = [:name,:system,:description,:tenant_id]
      else
        if params[:parent] || params[:parent_id]
          # This should arguably be UPDATE
          @parent = find_key_cap(model,params[:parent] || params[:parent_id],cap("READ"))
        elsif !params[:system]
          @parent = visible(model,cap("READ")).find_by!(system: true)
        end
        params[:parent_id] = @parent.id
      end
      params[:tenant_id] ||= @current_user.current_tenant_id
      params.require(:name)
      validate_create(params[:tenant_id])
      @deployment = Deployment.create!(params.permit(*permits))
      if params[:system]
        @deployment.update_attributes!(state: Deployment::COMMITTED)
      end
    end
    respond_to do |format|
      format.html { redirect_to deployment_path(@deployment.id)}
      format.json { render api_show @deployment }
    end
  end

  def update
    Deployment.transaction do
      @deployment = find_key_cap(model,params[:id],cap("UPDATE")).lock!
      simple_update(@deployment,%w{name description tenant_id})
    end
    respond_to do |format|
      format.html { redirect_to deployment_path(@deployment.id) }
      format.json { render api_show @deployment }
    end
  end

  def destroy
    model.transaction do
      @deployment = find_key_cap(model, params[:id], cap("DESTROY"))
      @deployment.destroy
    end
    respond_to do |format|
      format.html { redirect_to deployment_path(@deployment.parent_id) }
      format.json { render api_delete @deployment }
    end
  end

  # GET /api/status/deployments
  def status
    deployment = find_key_cap(model, params[:id], cap("READ")) rescue nil
    out = {
      node_roles: {},
      id: -1,
      state: -1,
      md5: '',
      nodes: -1,
      status: 'unknown'
    }

    NodeRole.transaction do  # performance optimization

      nr = if deployment
        out[:md5] = deployment.node_role_md5
        out[:state] = deployment.state
        out[:status] = Deployment::STATES[deployment.state]
        out[:id] = deployment.id
        out[:nodes] = deployment.nodes
        deployment.node_roles
      else
        out[:nodes] = Node.count
        NodeRole.all
      end

      nr.each do |role|
        state = role.state
        #state = rand(4) #testing random states (for updating)
        out[:node_roles][role.id] = {
          status: NodeRole::STATES[state],
          state: state
        }
      end

    end

    render api_array out.to_json
  end

  def anneal
    model.transaction do
      @deployment = find_key_cap(model, params[:deployment_id], cap("ANNEAL"))
      @list = @deployment.node_roles.
              visible(cap("READ","NODE"),@current_user.id).
              where(state: NodeRole::TRANSITION).order("cohort,id")
    end
    respond_to do |format|
      format.html {  }
      format.json { render api_index NodeRole, @list }
    end
  end

  def cohorts
    respond_to do |format|
      format.html {
        model.transaction do
          @deployment = find_key_cap(model params[:deployment_id], cap("READ"))
          @roles = @deployment.deployment_roles.sort{|a,b|a.role.cohort <=> b.role.cohort}
          # alpha lists by ID
          @nodes = Node.visible(cap("READ","NODE"),@current_user.id).
                   order("name ASC").select do |n|
            (n.deployment_id == @deployment.deployment_id) ||
              (n.node_roles.where(:deployment_id => @deployment.id).count > 0)
          end
        end
      }
    end

  end

  def propose
    model.transaction do
      @deployment = find_key_cap(model, params[:deployment_id], cap("PROPOSE"))
      @deployment.propose
    end
    respond_to do |format|
      format.html { redirect_to deployment_path(@deployment.id) }
      format.json { render api_show @deployment }
    end
  end

  def commit
    model.transaction do
      @deployment = find_key_cap(model, params[:deployment_id], cap("COMMIT"))
      @deployment.commit
    end
    respond_to do |format|
      format.html { redirect_to deployment_path(@deployment.id) }
      format.json { render api_show @deployment }
    end
  end

  def recall
    propose
  end

  # PUT
  # calls redeploy on all nodes in deployment
  def redeploy
    @deployment = find_key_cap(model,
                               params[:id] || params[:name] || params[:deployment_id],
                               cap("REDEPLOY"))
    # Not in a transaction due to redeploy call.
    @deployment.nodes.visible(cap("REDEPLOY","NODE"),@current_user.id).each { |n| n.redeploy! }
    render api_show @deployment
  end

end
