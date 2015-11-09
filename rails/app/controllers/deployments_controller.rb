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

  def sample
    render api_sample(Deployment)
  end
  
  def match
    attrs = Deployment.attribute_names.map{|a|a.to_sym}
    objs = []
    ok_params = params.permit(attrs)
    objs = Deployment.where(ok_params) if !ok_params.empty?
    respond_to do |format|
      format.html {}
      format.json { render api_index Deployment, objs }
    end
  end
  
  def index
    @list = Deployment.order("id DESC").all
    respond_to do |format|
      format.html { }
      format.json { render api_index Deployment, @list }
    end
  end

  def show
    @deployment = Deployment.find_key params[:id]
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
    if params[:parent] || params[:parent_id]
      @parent = Deployment.find_key(params[:parent] || params[:parent_id])
    else
      @parent = Deployment.system
    end
    params[:parent_id] = @parent.id
    params.require(:name)
    params.require(:parent_id)
    @deployment = Deployment.create!(params.permit(:name,:parent_id,:description))
    respond_to do |format|
      format.html { redirect_to deployment_path(@deployment.id)}
      format.json { render api_show @deployment }
    end
  end

  def update
    Deployment.transaction do
      @deployment = Deployment.find_key(params[:id]).lock!
      if request.patch?
        patch(@deployment,%w{name description})
        render api_show @deployment
      else
        @deployment.update_attributes!(params.permit(:name,:description))
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
    @deployment.destroy
    respond_to do |format|
      format.html { redirect_to deployment_path(@deployment.parent_id) }
      format.json { render api_delete @deployment }
    end
  end

  def status 
    deployment = Deployment.find_key params[:id]

    out = {
      node_roles: {},
      id: -1,
      state: -1,
      md5: '',
      status: 'unknown'
    }

    nr = if deployment
      out[:md5] = deployment.node_role_md5
      out[:state] = deployment.state
      out[:status] = Deployment::STATES[deployment.state]
      out[:id] = deployment.id
      deployment.node_roles
    else
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


    render api_array out.to_json
  end

  def monitor

    @deployment = Deployment.find_key params[:id]
    raise "deployment not found" unless @deployment

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
            status: NodeRole::STATES[state],
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
    @list = NodeRole.peers_by_state(@deployment, NodeRole::TRANSITION).order("cohort,id")
    respond_to do |format|
      format.html {  }
      format.json { render api_index NodeRole, @list }
    end
  end

  def cohorts
    @deployment = Deployment.find_key params[:deployment_id]
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

  def graph
    @deployment = Deployment.find_key params[:deployment_id]
    respond_to do |format|
      format.html {  }
      format.json {
        graph = []
        @deployment.node_roles.each do |nr|
          vertex = { "id"=> nr.id, "name"=> "#{nr.node.name}: #{nr.role.name}", "data"=> {"$color"=>"#83548B"}, "$type"=>"square", "$dim"=>15, "adjacencies" =>[] }
          nr.children.each do |c|
            vertex["adjacencies"] << { "nodeTo"=> c.id, "nodeFrom"=> nr.id, "data"=> { "$color" => "#557EAA" } }
          end
          graph << vertex
        end
        render :json=>graph.to_json, :content_type=>cb_content_type(:list) 
      }
    end
  end

  def propose
    @deployment = Deployment.find_key params[:deployment_id]
    @deployment.propose
    respond_to do |format|
      format.html { redirect_to deployment_path(@deployment.id) }
      format.json { render api_show @deployment }
    end
  end

  def commit
    @deployment = Deployment.find_key params[:deployment_id]
    @deployment.commit
    respond_to do |format|
      format.html { redirect_to deployment_path(@deployment.id) }
      format.json { render api_show @deployment }
    end
  end

  def recall
    propose
  end

end
