# Copyright 2014, Dell
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
class NodesController < ApplicationController

  # API GET /rebar/v2/nodes
  # UI GET /dashboard

  def sample
    render api_sample(Node)
  end

  def match
    attrs = Node.attribute_names.map{|a|a.to_sym}
    objs = []
    ok_params = params.permit(attrs)
    objs = validate_match(ok_params, :tenant_id, "NODE", Node)
    respond_to do |format|
      format.html {}
      format.json { render api_index Node, objs }
    end
  end

  def index
    @list = case
            when params.has_key?(:group_id)
              Group.find_key(params[:group_id]).nodes
            when params.has_key?(:deployment_id)
              Deployment.find_key(params[:deployment_id]).nodes
            when params.has_key?(:role_id)
              Role.find_key(params[:role_id]).nodes
            when params.has_key?(:deployment_role_id)
              DeploymentRole.find_key(params[:deployment_role_id]).nodes
            when params.has_key?(:provider_id)
              Provider.find_key(params[:provider_id]).nodes
            else
              Node.all
            end
    @list = @list.to_a
    t_ids = build_tenant_list("NODE_READ")
    @list.delete_if { |x| !t_ids.include? x.tenant_id }
    respond_to do |format|
      format.html { @list.to_a.delete_if { |n| n.system } }
      format.json { render api_index Node, @list }
    end
  end

  # API /api/status/nodes(/:id)
  def status

    status = {}
    Node.transaction do   # performance optimization

      nodes = if params[:id]
        node = Node.find_key params[:id]
        nodes = Node.where(:id=>node.id).to_a
      else
        nodes = Node.non_system.to_a
      end
      t_ids = build_tenant_list("NODE_READ")
      nodes.delete_if { |x| !t_ids.include? x.tenant_id }
      nodes.each do |n|
        state = n.state
        str = [
          t(n.alive ? "common.state.alive" : "common.state.dead"),
          t(n.available ? "common.state.available" : "common.state.reserved"),
          NodeRole.state_name(state)
        ].join("\n")
        status[n.id] = {
          :name => n.name,
          :state => state,
          :status => NodeRole::STATES[state],
          :strStatus => str
        }
      end

    end

    render api_array status.to_json

  end

  def show
    @node = Node.find_key params[:id]
    validate_read(@node.tenant_id, "NODE", Node, params[:id])
    respond_to do |format|
      format.html {  } # show.html.erb
      format.json { render api_show @node }
    end
  end

  # Get the addresses allocated to a node on a network.
  def addresses
    nodename = params[:node_id]
    @node = Node.find_key(nodename)
    validate_read(@node.tenant_id, "NODE", Node, params[:node_id])
    if params[:network]
      @net = Network.find_key(params[:network])
      validate_read(@net.tenant_id, "NETWORK", Network, params[:network])
      res = {
        "node" => @node.name,
        "network" => @net.name,
        "category" => @net.category,
        "addresses" => @net.node_allocations(@node).map{|a|a.to_s}
      }
      render :json => res, :content_type=>cb_content_type(:addresses, "object")
    else
      res = []

      if params[:category]
        nets = Network.in_category(params[:category]).to_a
      else
        nets = Network.all.to_a
      end
      t_ids = build_tenant_list("NETWORK_READ")
      nets.delete_if { |x| !t_ids.include? x.tenant_id }

      nets.each do |n|
        ips = n.node_allocations(@node)
        next if ips.empty?

        res << {
            "node" => @node.name,
            "network" => n.name,
            "category" => n.category,
            "addresses" => ips.map{|a|a.to_s}
        }
      end

      render :json => res, :content_type=>cb_content_type(:addresses, "array")
    end
  end

  # RESTful DELETE of the node resource
  def destroy
    @node = Node.find_key(params[:id] || params[:name])
    if params[:group_id]
      validate_read(@node.tenant_id, "NODE", Node, node.id)
      g = Group.find_key params[:group_id]
      validate_destroy(g.tenant_id, "Group", Group, params[:group_id])
      g.nodes.delete(@node) if g.nodes.include? @node
      render :text=>I18n.t('api.removed', :item=>'node', :collection=>'group')
    else
      validate_destroy(@node.tenant_id, "NODE", Node, @node.id)
      @node.destroy
      respond_to do |format|
        format.html { redirect_to deployment_path(@node.deployment_id) }
        format.json { render api_delete @node }
      end
    end
  end

  def power
    @node = Node.find_key(params[:id] || params[:name] || params[:node_id])
    if request.put?
      params.require(:poweraction)
      @poweraction = params[:poweraction].to_sym
      if @node.power.include? @poweraction
        result = @node.power.send(@poweraction) rescue nil
        render api_not_implemented(@node, @poweraction, "see logs for internal error") if result.nil?
        render api_result({"id" => @node.id, "action" => @poweraction, "result" => result })
      else
        render api_not_implemented @node, @poweraction, @node.power.keys
      end
    elsif request.get?
      render api_array @node.power.keys
    end

  end

  def debug
    node_action :debug, "UPDATE"
  end

  def undebug
    node_action :undebug, "UPDATE"
  end

  def redeploy
    node_action :redeploy!, "REDEPLOY"
  end

  def scrub
    node_action :scrub!, "SCRUB"
  end

  def propose
    node_action :propose!, "PROPOSE"
  end

  def commit
    node_action :commit!, "COMMIT"
  end

  # RESTfule POST of the node resource
  def create
    if params[:group_id] and params[:node_id]
      g = Group.find_key params[:group_id]
      n = Node.find_key params[:node_id]
      validate_read(n.tenant_id, "NODE", Node, params[:node_id])
      validate_update(g.tenant_id, "GROUP", Group, params[:group_id])
      n.groups << g if g and n
      render :text=>I18n.t('api.added', :item=>g.name, :collection=>'node.groups')
    else
      params[:deployment_id] = Deployment.find_key(params[:deployment]).id if params.has_key? :deployment
      params[:deployment_id] ||= Deployment.system
      params[:provider_id] = Provider.find_key(params[:provider]).id if params.has_key? :provider
      params[:provider_id] ||= Provider.find_by!(name: 'metal').id
      params.require(:name)
      params.require(:deployment_id)
      params.require(:provider_id)
      unless params[:tenant_id]
        params[:tenant_id] = @current_user.tenant_id
      end
      validate_create(params[:tenant_id], "NODE", Node)
      hints = params[:hints] || {}
      Rails.logger.info("Node create params: #{params.inspect}")
      default_net = nil
      Node.transaction do
        @node = Node.create!(params.permit(:name,
                                           :description,
                                           :admin,
					   :tenant_id,
                                           :deployment_id,
                                           :provider_id,
                                           :allocated,
                                           :alive,
                                           :system,
                                           :available,
                                           :bootenv,
                                           :variant,
                                           :arch,
                                           :os_family))
        # Keep suport for mac and ip hints in short form around for legacy Sledgehammer purposes
        default_net = Network.lookup_network(params[:ip]) if params[:ip]
        hints["hint-#{default_net.name}-v4addr"] = params[:ip] if default_net
        hints["node-control-address"] = params[:ip] if params[:ip]
        # Kubernetes hack for networking until we get better at it.
        hints["node-private-control-address"] = params[:ip] if params[:ip]
        hints["hint-admin-macs"] = [params[:mac]] if params[:mac]
        # Set any hints we got with node creation.
        hints.each do |k,v|
          Attrib.set(k,@node,v)
        end
      end
      default_net.make_node_role(@node) if default_net
      render api_show @node
    end
  end

  def update
    Node.transaction do
      @node = Node.find_key(params[:id]).lock!
      validate_update(@node.tenant_id, "NODE", Node, params[:id])
      if request.patch?
        patch(@node,%w{name description target_role_id deployment_id allocated available alive bootenv})
      else
        params[:node_deployment].each { |k,v| params[k] = v } if params.has_key? :node_deployment
        params[:deployment_id] = Deployment.find_key(params[:deployment]).id if params.has_key? :deployment
        if params.has_key?(:deployment_id) && params.has_key?(:old_deployment_id)
          old_deployment = Deployment.find_key(params[:old_deployment_id])
          raise "Node is not in old deployment #{params[:old_deployment_id]}" unless @node.deployment == old_deployment
        end
        @node.update_attributes!(params.permit(:name,
                                               :description,
                                               :target_role_id,
					       :tenant_id,
                                               :deployment_id,
                                               :allocated,
                                               :available,
                                               :alive,
                                               :bootenv))
      end
    end
    render api_show @node
  end

  #test_ methods support test functions that are not considered stable APIs
  def test_load_data

    @node = Node.find_key params[:id]
    # get the file
    file = File.join "test", "data", (params[:source] || "node_discovery") + ".json"
    raw = File.read file
    # cleanup
    mac = 6.times.map{ |i| rand(256).to_s(16) }.join(":")
    raw = raw.gsub /00:00:00:00:00:00/, mac
    # update the node
    json = JSON.load raw
    @node.discovery  = json
    @node.save!
    render api_show @node

  end

  private

  def node_action(meth, action)
    @node = Node.find_key(params[:id] || params[:name] || params[:node_id])
    validate_action(@node.tenant_id, "NODE", Node, @node.id, action)
    @node.send(meth)
    render api_show @node
  end

end
