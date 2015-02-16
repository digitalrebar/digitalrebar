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
    @deployment = Deployment.find_key params[:id]
    @deployment.update_attributes!(params.permit(:name,:description))
    render api_show @deployment
  end

  def destroy
    @deployment = Deployment.find_key params[:id]
    @deployment.destroy
    render api_delete @deployment
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
          vertex = { "id"=> nr.id, "name"=> "#{nr.node.alias}: #{nr.role.name}", "data"=> {"$color"=>"#83548B"}, "$type"=>"square", "$dim"=>15, "adjacencies" =>[] }
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
