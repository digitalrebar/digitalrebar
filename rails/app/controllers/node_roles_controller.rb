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
  self.model = NodeRole
  self.cap_base = "NODE"

  # GET /api/status/node_roles?age=[newer than # seconds]
  # PUT same URL with added payload {"nodes":[1,2,3]} to find deleted nodes
  def status

    # by design, this informs the API about REST objects that have been updated
    # it is NOT indended to return the data - the consumer needs to make that decision

    out = { changed: { nodes: [], node_roles: [], deployments: [], providers: [] }, deleted: { nodes: [], deployments: [], providers: [] } }
    recent = (params[:age] || 300).to_i

    changed_nodes = []
    changed_deployments = []
    NodeRole.transaction do
      visible(model, cap("READ")).each do |nr|
        age = Time.now - nr.updated_at
        next if nr.state >= 0 && age >= recent
        changed_nodes << nr.node_id
        changed_deployments << nr.deployment_id
        out[:changed][:node_roles] << nr.id
      end
      # optimization avoid logic during the loop
      out[:changed][:nodes] = changed_nodes.uniq
      out[:changed][:deployments] = changed_deployments.uniq

      visible(Provider, cap("READ","PROVIDER")).each do |p|
        age = Time.now - p.updated_at
        next if age >= recent
        out[:changed][:providers] << p.id
      end

      # on PUT, to handle UX not knowing about deleted nodes
      # compare nodes to passed nodes json list.
      # return the deleted ones that are not currently nodes from the UX list
      if request.put? and params[:nodes]
        nodes = visible(Node,cap("READ","NODE")).map{ |n| n.id }
        out[:deleted][:nodes] = (params[:nodes] - nodes) rescue []
      end
      if request.put? and params[:deployments]
        deployments = visible(Deployment, cap("READ","DEPLOYMENT")).map{ |d| d.id }
        out[:deleted][:deployments] = (params[:deployments] - deployments) rescue []
      end
      if request.put? and params[:providers]
        providers = visible(Provider, cap("READ","PROVIDER")).map{ |p| p.id }
        out[:deleted][:providers] = (params[:providers] - providers) rescue []
      end
    end
    # done
    render api_array out.to_json
  end

  def index
    NodeRole.transaction do
      @list = if params.key? :node_id
                find_key_cap(Node, params[:node_id],cap("READ")).node_roles
              elsif params.key? :deployment_id
                find_key_cap(Deployment,params[:deployment_id], cap("READ","DEPLOYMENT")).
                  node_roles.visible(cap("READ"),@current_user.id)
              else
                visible(model,cap("READ"))
              end.order("cohort asc, id asc")
      if params.has_key? :runlog
        @list = @list.to_a
        limit = params[:runlog] || "80"
        @list.each do |i|
          i.runlog = i.runlog.truncate(limit.to_i) unless i.state == NodeRole::ERROR
        end
      end
    end
    respond_to do |format|
      format.html { }
      format.json { render api_index NodeRole, @list }
    end
  end

  def show
    @node_role = if params.key? :node_id
                   NodeRole.transaction do
                     node = find_key_cap(Node, params[:node_id],cap("READ"))
                     # This will need updating once we care about ROLE_READ
                     role = Role.find_key(params[:id])
                     node.node_roles.find_by!(role_id: role.id)
                   end
                 else
                   find_key_cap(model, params[:id], cap("READ"))
                 end
    respond_to do |format|
      format.html {  }
      format.json { render api_show @node_role }
    end
  end

  def create
    node = nil
    role = nil
    depl = nil
    model.transaction do
      node = find_key_cap(Node, params[:node] || params[:node_id], cap("UPDATE"))
      # alternate formatting of input from bootstrap. Do we still need this?
      nr_roles = params.key?(:node_roles) ? params[:node_roles][:role_id] : nil
      # Make sure the role we want to add is in a tenant we have NODE_UPDATE caps in.
      # Arguably, this should be ROLE_READ, but whatever.
      role = Role.find_key(params[:role] || params[:role_id] || nr_roles)
      # Ditto for the deployment, except that it should be DEPLOYMENT_UPDATE
      depl = find_key_cap(Deployment,
                          params[:deployment_id] ||
                          params[:deployment] ||
                          node.deployment_id,
                          cap("UPDATE","DEPLOYMENT"))
    end
    begin
      # For proper sequencing, safe_create must be called outside a transaction.
      @node_role = NodeRole.safe_create!(role_id: role.id,
                                         # node.tenant_id, maybe?
                                         tenant_id: depl.tenant_id,
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
    NodeRole.transaction do
      @node_role = if !params.key?(:node_id)
                     find_key_cap(model, params[:id], cap("UPDATE"))
                   elsif key.is_a?(Fixnum) or key.is_a?(Integer) or key =~ /^[0-9]+$/
                     # I am not terribly sure about this logic.  Seems messed up.
                     find_key_cap(model,key,cap("UPDATE"))
                   else
                     node = find_key_cap(Node, params[:node_id],cap("UPDATE"))
                     role = Role.find_by!(name: key)
                     visible(model,cap("UPDATE")).find_by!(node_id: node.id, role_id: role.id)
                   end.lock!
      # if you've been passed data then save it
      if request.patch?
        patch(@node_role, %w{data run_count tenant_id})
      else
        if params[:data]
          @node_role.data = params[:data]
          @node_role.save!
        end
      end
    end
    respond_to do |format|
      format.html { flash[:notice] = I18n.t 'saved', :scope=>'layouts.node_roles.show'; render 'show' }
      format.json { render api_show @node_role }
    end
  end

  def destroy
    model.transaction do
      @node_role = find_key_cap(model, params[:id] || params[:node_role_id], cap("DESTROY"))
      @node_role.destroy
    end
    respond_to do |format|
      format.html { redirect_to deployment_path(@node_role.deployment_id) }
      format.json { render api_delete @node_role }
    end
  end

  def propose
    model.transaction do
      @node_role = find_key_cap(model, params[:node_role_id], cap("PROPOSE"))
      @node_role.propose!
    end
    respond_to do |format|
      format.html { redirect_to node_role_path(@node_role.id) }
      format.json { render api_show @node_role }
    end
  end

  def commit
    model.transaction do
      @node_role = find_key_cap(model, params[:node_role_id], cap("COMMIT"))
      @node_role.commit!
    end
    respond_to do |format|
      format.html { redirect_to node_role_path(@node_role.id) }
      format.json { render api_show @node_role }
    end
  end

  def retry
    model.transaction do
      @node_role = find_key_cap(model, params[:id] || params[:node_role_id], cap("RETRY"))
      @node_role.todo!
    end
    respond_to do |format|
      format.html { redirect_to node_role_path(@node_role.id) }
      format.json { render api_show @node_role }
    end
  end

  def parents
    @list = find_key_cap(model, params[:node_role_id], cap("READ")).
            parents.visible(cap("READ"), @current_user.id)
    render api_index NodeRole, @list
  end

  def children
    @list = find_key_cap(model, params[:node_role_id], cap("READ")).
            children.visible(cap("READ"), @current_user.id)
    render api_index NodeRole, @list
  end

  def anneal
    status = { :json => { "message" => "finished" }, :state => 200 }
    NodeRole.transaction do
      nrs = visible(model, cap("READ")).committed
      status = case
               when nrs.in_state(NodeRole::ERROR).count > 0
                 { :json => { "message" => "failed" }, :status => 409 }
               when nrs.in_state(NodeRole::TRANSITION).count > 0
                 { :json => { "message" => "working" }, :status => 202 }
               when nrs.in_state(NodeRole::TODO).count > 0
                 { :json => { "message" => "scheduled" }, :status => 202 }
               end
    end
    respond_to do |format|
      format.html { }
      format.json { render status }
    end
  end

end
