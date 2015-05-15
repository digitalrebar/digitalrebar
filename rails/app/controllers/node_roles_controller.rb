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

class NodeRolesController < ApplicationController

  def index
    @list = (if params.key? :node_id
              Node.find_key(params[:node_id]).node_roles
            elsif params.key? :deployment_id
              Deployment.find_key(params[:deployment_id]).node_roles
            else
              NodeRole.all
            end).order("cohort asc, id asc")
    respond_to do |format|
      format.html { }
      format.json { render api_index NodeRole, @list }
    end
  end

  def show
    if params.key? :node_id
      node = Node.find_key params[:node_id]
      raise "could not find node #{params[:node_id]}" unless node
      role = Role.find_key params[:id]
      raise "could not find role #{params[:id]}" unless role
      @node_role = NodeRole.find_by!(node_id: node.id, role_id: role.id)
    else
      @node_role = NodeRole.find_key params[:id]
    end
    respond_to do |format|
      format.html {  }
      format.json { render api_show @node_role }
    end
  end

  def create
    # helpers to allow create by names instead of IDs
    depl = nil
    if params.key? :deployment_id
      depl = Deployment.find_key(params[:deployment_id])
    elsif params.key? :deployment
      depl = Deployment.find_key(params[:deployment])
    end
    # alternate formatting of input from bootstrap
    if params.key? :node_roles
      nr_roles = params[:node_roles][:role_id]
    end
    node = Node.find_key(params[:node] || params[:node_id])
    role = Role.find_key(params[:role] || params[:role_id] || nr_roles)
    depl ||= node.deployment
    begin
      @node_role = NodeRole.safe_create!(role_id: role.id,
                                         node_id: node.id,
                                         deployment_id: depl.id)
    rescue StandardError => e
      Rails.logger.fatal("Exception on safe_create!: #{e.message}")
      Rails.logger.fatal(e.backtrace)
      raise e
    end
    if params[:data]
      @node_role.data = params[:data]
      @node_role.save!
    end
    respond_to do |format|
      format.html { redirect_to deployment_path(depl.id) }
      format.json { render api_show @node_role }
    end

  end

  def update
    # we're being called from /nodes path we may get the name instead of ID
    key = params[:id]
    @node_role = if params.key? :node_id
      node = Node.find_key params[:node_id]
      if key.is_a?(Fixnum) or key.is_a?(Integer) or key =~ /^[0-9]+$/
        NodeRole.find key
      else
        role = Role.find_by :name=>key
        NodeRole.find_by :node_id=>node.id, :role_id=>role.id
      end
    else
      NodeRole.find_key key
    end
    # if you've been passed data then save it
    if params[:data]
      NodeRole.transaction do
        @node_role.data = params[:data]
        @node_role.save!
        flash[:notice] = I18n.t 'saved', :scope=>'layouts.node_roles.show'
      end
    end
    respond_to do |format|
      format.html { render 'show' }
      format.json { render api_show @node_role }
    end
  end

  def destroy
    @node_role = NodeRole.find_key (params[:id] || params[:node_role_id])
    @node_role.destroy
    respond_to do |format|
      format.html { redirect_to deployment_path(@node_role.deployment_id) }
      format.json { render api_delete @node_role }
    end
  end

  def propose
    @node_role = NodeRole.find_key params[:node_role_id]
    @node_role.propose!
    respond_to do |format|
      format.html { redirect_to node_role_path(@node_role.id) }
      format.json { render api_show @node_role }
    end
  end

  def commit
    @node_role = NodeRole.find_key params[:node_role_id]
    @node_role.commit!
    respond_to do |format|
      format.html { redirect_to node_role_path(@node_role.id) }
      format.json { render api_show @node_role }
    end
  end


  def retry
    params[:id] ||= params[:node_role_id]
    @node_role = NodeRole.find_key params[:id]
    @node_role.todo!
    respond_to do |format|
      format.html { redirect_to node_role_path(@node_role.id) }
      format.json { render api_show @node_role }
    end

  end

  def anneal
    respond_to do |format|
      format.html { }
      format.json {
        if NodeRole.committed.in_state(NodeRole::TODO).count > 0
          render :json => { "message" => "scheduled" }, :status => 202
        elsif NodeRole.committed.in_state(NodeRole::TRANSITION).count > 0
          render :json => { "message" => "working" }, :status => 202
        elsif NodeRole.committed.in_state(NodeRole::ERROR).count > 0
          Rails.logger.info("Failed node roles: #{NodeRole.committed.in_state(NodeRole::ERROR).inspect}")
          render :json => { "message" => "failed" }, :status => 409
        else
          render :json => { "message" => "finished" }, :state => 200
        end
      }
    end
  end

end

