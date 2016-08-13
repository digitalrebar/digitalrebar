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
    end

    internal_batch(@deployment.id) if params[:nodes]

    if params[:system] || params[:commit]
      @deployment = find_key_cap(model, @deployment.id, cap("COMMIT"))
      @deployment.commit
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
      if @deployment.system? # "cannot destroy system deployments" 
        api_not_supported("delete", @deployments)
      else
        @deployment.destroy
      end
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

  # UX ONLY -> REMOVE???
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

  # view only
  def template

    @source = find_key_cap(model, (params[:source] || 'system'), cap("READ"))
    @provider = visible(Provider, cap("READ")).order("id DESC")
    @deployment = find_key_cap(model, params[:deployment_id], cap("PROPOSE"))

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

  def internal_batch(did)
    roles = {}
    deployment_roles = []
    deployment = nil

    Deployment.transaction do
      deployment = find_key_cap(model, did, cap("UPDATE"))
      api_error(deployment,"put", "Deployment must be proposed") unless deployment.proposed?
      api_error(deployment,"put", "Nodes List is required") unless params["nodes"]

      # collect the attributes for the batch to make sure we have the associated roles
      if params["attribs"]
        params["attribs"].each do |a, v|
          attrib = Attrib.find_key a
          roles[attrib.role.name] = { id: attrib.role.id, nodes: [], cohort: attrib.role.cohort }
        end
      end

      Rails.logger.debug("Deployment Batch: starting...#{deployment.name}")

      # review nodes
      params["nodes"].each_with_index do |node, block|

        Rails.logger.debug("Deployment Batch: working on #{node}")

        # collect the roles, take no action
        node["roles"].each do |r|
          role = find_key_cap(Role,r, cap("READ","ROLE"))
          roles[r] = { id: role.id, cohort: role.cohort, nodes: [] }
        end

        # collect/create nodes - if you have an ID or it's >0, then use it
        if node["id"] and (node["id"].to_i > -1)
          # collect nodes per deployment
          n = find_key_cap(Node,node["id"], cap("UPDATE","NODE"))
          node["roles"].each { |r| roles[r][:nodes] << n.id }
          # and put node in deployment if it's not
          if n.deployment_id != deployment.id
            n.deployment_id = deployment.id
            n.save!
          end
          Rails.logger.debug("Deployment Batch: added node #{n.name} to deployment #{deployment.name}")

        # create nodes, if no count then assume 1
        else
          for i in 1..(node["count"] || 1)
            # provider is global BUT can be overridden per node
            api_error(deployment,"put", "Provider required") unless node["provider"] || params["provider"]
            pro_raw = node["provider"] || params["provider"]
            api_error(deployment,"put", "Provider name required") unless pro_raw["name"]

            Rails.logger.debug("Deployment Batch: checking to see if user has access to provider #{pro_raw['name']}")
            provider = find_key_cap(Provider,(pro_raw["name"]), cap("READ","PROVIDER"))

            name = "#{node["prefix"] || deployment.name}-#{(block+1).to_s.rjust(2, "0")}-#{i.to_s.rjust(3, "0")}.#{provider.name}.#{params["tld"] || deployment.name + '.cloud'}"
            Rails.logger.debug("Deployment Batch: adding node #{name}")
            validate_create(@current_user.current_tenant_id, cap("CREATE"), Node)
            newnode = Node.create!( name: name,
                                  description: node["description"] || t("useradded", :user => @current_user.username),
                                  admin: false,
                                  tenant_id: @current_user.current_tenant_id,
                                  deployment_id: deployment.id,
                                  provider_id: provider.id,
                                  allocated: false,
                                  alive: false,
                                  system: false,
                                  available: true)
            # add hints
            hints = {
              'use-proxy' => false,
              'use-ntp' => false,
              'use-dns' => false,
              'use-logging' => false,
              'provider-create-hint' => {
                'hostname' => name
              }
            }
            node_hints = params["provider"]["hints"] || node["provider"]["hints"] || {}
            node_hints.each do |hint, value|
              hints["provider-create-hint"][hint] = value
            end
            Rails.logger.debug("Deployment Batch: adding hints to node #{name}: #{node_hints}")
            hints.each do |k,v|
              Attrib.set(k,newnode,v)
            end
            # add the node to the roles that should be applied
            node["roles"].each { |r| roles[r][:nodes] << newnode.id }
          end
        end
      end # nodes loop

      # we need to do this in cohort order!
      roles = roles.sort_by { |k, r| r[:cohort] }

      # add roles to deployment
      roles.each do |name, r|
        Rails.logger.debug("Deployment Batch: adding role #{name} deployment #{deployment.name}")
        role = find_key_cap(Role, name, cap("READ","ROLE"))
        deployment_roles << role.add_to_deployment(deployment)
      end # roles loop

      Rails.logger.debug("Deployment Batch: deployment_roles #{deployment_roles.inspect}")

      # set the attributes
      if params["attribs"]
        params["attribs"].each do |a, v|
          Attrib.set(a, deployment, v)
        end
      end

    end # end Transaction

    # assign nodes roles to deployment (cannot be in the top transaction)
    roles.each do |name, r|
      role = find_key_cap(Role, name, cap("READ","ROLE"))
      Rails.logger.debug("Deployment Batch: adding role #{name} nodes #{r[:nodes]}")
      r[:nodes].each do |n|
        node = find_key_cap(Node, n, cap("READ", "NODE"))
        role.add_to_node node
      end
    end # roles loop

  end


  # PUT deployments/<id>/batch - id must exist and be in commit state
  #
  # bulk load deployment information
  def batch
    internal_batch(params[:deployment_id])

    if params[:commit]
      deployment = find_key_cap(model, params[:deployment_id], cap("COMMIT"))
      deployment.commit
    end

    render api_show deployment

  end

end
