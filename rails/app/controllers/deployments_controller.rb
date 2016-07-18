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
    tenant_ids = build_tenant_list("DEPLOYMENT_READ")
    @list = Deployment.where(tenant_id: tenant_ids).order("id DESC").all
    respond_to do |format|
      format.html { }
      format.json { render api_index Deployment, @list }
    end
  end

  def show
    @deployment = Deployment.find_key params[:id]
    validate_read(@deployment.tenant_id, "DEPLOYMENT", Deployment, params[:id])
    respond_to do |format|
      format.html {
        @roles = @deployment.deployment_roles.sort{|a,b|a.role.cohort <=> b.role.cohort}
        # remove the service roles
        @roles.delete_if { |r| r.role.service }
        # alpha lists by ID
        @nodes = Node.order("name ASC").select do |n|
          (!n.is_system? and
          (n.deployment_id == @deployment.id) ||
          (n.node_roles.where(:deployment_id => @deployment.id).count > 0))
        end
      }
      format.json { render api_show @deployment }
    end
  end

  def create
    if ! params[:system]
      if params[:parent] || params[:parent_id]
        @parent = Deployment.find_key(params[:parent] || params[:parent_id])
      elsif !params[:system]
        @parent = Deployment.system
      end
      params[:parent_id] = @parent.id
      permits = [:name,:parent_id,:description,:tenant_id]
    elsif Deployment.find_by(system: true)
      raise "Only one system deployment permitted"
    else
      permits = [:name,:system,:description,:tenant_id]
    end
    unless params[:tenant_id]
      params[:tenant_id] = @current_user.current_tenant_id
    end
    params.require(:name)
    validate_create(params[:tenant_id], "DEPLOYMENT", Deployment)
    Deployment.transaction do
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
      @deployment = Deployment.find_key(params[:id]).lock!
      validate_update(@deployment.tenant_id, "DEPLOYMENT", Deployment, params[:id])
      if request.patch?
        patch(@deployment,%w{name description tenant_id})
        render api_show @deployment
      else
        @deployment.update_attributes!(params.permit(:name,:description,:tenant_id))
        respond_to do |format|
          format.html do
            redirect_to deployment_path(@deployment.id)
          end
          format.json do
            render api_show @deployment
          end
        end
      end
    end
  end

  def destroy
    @deployment = Deployment.find_key params[:id]
    validate_destroy(@deployment.tenant_id, "DEPLOYMENT", Deployment, params[:id])
    @deployment.destroy
    respond_to do |format|
      format.html { redirect_to deployment_path(@deployment.parent_id) }
      format.json { render api_delete @deployment }
    end
  end

  # GET /api/status/deployments
  def status
    deployment = Deployment.find_key params[:id] rescue nil
    if deployment
      unless capable(deployment.tenant_id, "DEPLOYMENT_READ")
        deployment = nil
      end
    end

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

  def monitor

    @deployment = Deployment.find_key params[:id]
    raise RebarNotFoundError.new(params[:id], Deployment) unless @deployment
    validate_read(@deployment.tenant_id, "DEPLOYMENT", Deployment, params[:deployment_id])

    respond_to do |format|
      format.html { }
      format.json {

        deployment_roles = DeploymentRole.where(deployment_id: @deployment.id).joins(:role).select("deployment_roles.*, roles.name as role_name, roles.cohort as role_cohort, roles.service as role_service").sort{|a,b|a.role_cohort <=> b.role_cohort}

        roles = deployment_roles.select { |r| !r.role_service }

        state = @deployment.state rescue Deployment::ERROR

        out = {
          roles: [],
          nodes: [],
          services: [],
          state: state,
          status: Deployment::STATES[state]
        }

        sn = @deployment.system_node
        out[:services] = NodeRole.where(node_id: sn.id).joins(:role).select("node_roles.*, roles.name as role_name").map do |service|
          state = service.state || NodeRole::ERROR
          {
            state: state,
            status: NodeRole::STATES[ state],
            path: node_role_path(service.id),
            name: service.role_name,
          }
        end

        roleHash = {}

        out[:roles] = roles.map do |role|
          roleHash[role.role_name] = role.id
          {
            name: role.role_name,
            id: role.id,
          }
        end

        node_roles = NodeRole.joins(:node,:role).where("nodes.system = 'f' and (nodes.deployment_id = #{@deployment.id} OR node_roles.deployment_id = #{@deployment.id})").select("node_roles.*, roles.name as role_name, nodes.name as node_name, nodes.admin as node_admin, nodes.available as node_avail, nodes.description as node_desc, nodes.alive as node_alive")

        nodes = {}
        node_roles.each do |nr|
          n = nodes[nr.node_name]
          unless n
            n = {
              name: nr.node_name,
              admin: nr.node_admin,
              id: nr.node_id,
              roles: {},
              description: nr.node_desc,
              path: node_path(nr.node_id)
            }
            n[:led] = if !nr.node_avail
              nr.node_alive ? 'reserved' : 'idle'
            elsif !nr.node_alive
              'off'
            else
              'on'
            end
            nodes[nr.node_name] = n
          end

          n[:roles][roleHash[nr.role_name]] = {
            state: nr.state,
            status: NodeRole::STATES[nr.state],
            id: nr.id,
            path: node_role_path(nr.role_id),
          }
        end

        nodes = nodes.values.sort{|a,b| a[:name] <=> b[:name]}
        admins = nodes.select{|n| n[:admin]}
        out[:nodes] = admins + (nodes - admins)

        render api_array out.to_json
      }
    end


  end

  def anneal
    @deployment = Deployment.find_key params[:deployment_id]
    validate_action(@deployment.tenant_id, "DEPLOYMENT", Deployment, params[:deployment_id], "ANNEAL")
    @list = NodeRole.peers_by_state(@deployment, NodeRole::TRANSITION).order("cohort,id")
    respond_to do |format|
      format.html {  }
      format.json { render api_index NodeRole, @list }
    end
  end

  def cohorts
    @deployment = Deployment.find_key params[:deployment_id]
    validate_read(@deplyment.tenant_id, "DEPLOYMENT", Deployment, params[:deployment_id])
    respond_to do |format|
      format.html {
        @roles = @deployment.deployment_roles.sort{|a,b|a.role.cohort <=> b.role.cohort}
        # alpha lists by ID
        @nodes = Node.order("name ASC").select do |n|
          (n.deployment_id == @deployment.deployment_id) ||
          (n.node_roles.where(:deployment_id => @deployment.id).count > 0)
        end
      }
    end

  end

  def propose
    @deployment = Deployment.find_key params[:deployment_id]
    validate_action(@deployment.tenant_id, "DEPLOYMENT", Deployment, params[:deployment_id], "PROPOSE")
    @deployment.propose
    respond_to do |format|
      format.html { redirect_to deployment_path(@deployment.id) }
      format.json { render api_show @deployment }
    end
  end

  def commit
    @deployment = Deployment.find_key params[:deployment_id]
    validate_action(@deployment.tenant_id, "DEPLOYMENT", Deployment, params[:deployment_id], "COMMIT")
    @deployment.commit
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
    @deployment = Deployment.find_key(params[:id] || params[:name] || params[:deployment_id])
    validate_action(@deployment.tenant_id, "DEPLOYMENT", Deployment, @deployment.id, "REDEPLOY")
    if @deployment
      Rails.logger.debug("Starting Deployment Redeploy for #{@deployment.name} with #{@deployment.nodes.count}")
      @deployment.nodes.each { |n| n.redeploy! }
      Rails.logger.debug("Ended Deployment Redeploy for #{@deployment.name}")
    end
    render api_show @deployment
  end

end
