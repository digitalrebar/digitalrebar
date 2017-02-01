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
  self.model = Node
  self.cap_base = "NODE"

  def whoami
    # For now, don't bother using the mac addresses
    # macs = params[:macs] || []
    Rails.logger.warn("Addrs: #{params[:addrs]}")
    addrs = params[:addrs] || []
    res = Node.where(["hint #>> '{provider,control_address}' in (?)",addrs])
    if res.length == 1
      render api_show(res[0])
    else
      # Giess based on connection information
      Rails.logger.warn("X-Forwarded-For: #{request.headers["X-Forwarded-For"]}")
      Rails.logger.warn("Remote IP: #{request.remote_ip}")
      src = request.headers["X-Forwarded-For"]
      src ||= request.remote_ip
      render api_show(NetworkAllocation.find_key(src).node)
    end
  end

  # We allow nodes to match on arbitrary node-bound attribs
  # as well as native model attributes.
  # This makes simple API queries easier to express
  def match
    attrs = model.attribute_names.map{|a|a.to_sym}
    ok_params = params.permit(attrs)
    mv = model.params_to_mv(params)
    objs = case
           when ok_params.empty? && mv.empty?
             model.where('false')
           when ok_params.empty? && !mv.empty?
             visible(model, cap("READ")).where_jsonb(mv)
           when mv.empty? && !ok_params.empty?
             visible(model, cap("READ")).where(ok_params)
           else
             visible(model, cap("READ")).where(ok_params).where_jsonb(mv)
           end
    respond_to do |format|
      format.html {}
      format.json { render api_index model, objs }
    end
  end

  def index
    @list = case
            when params.has_key?(:group_id)
              find_key_cap(Group,params[:group_id],cap("READ","GROUP")).nodes
            when params.has_key?(:deployment_id)
              find_key_cap(Deployment,params[:deployment_id],cap("READ","DEPLOYMENT")).nodes
            when params.has_key?(:role_id)
              find_key_cap(Role,params[:role_id],cap("READ","ROLE")).nodes
            when params.has_key?(:deployment_role_id)
              find_key_cap(DeploymentRole,params[:deployment_role_id], cap("READ","DEPLOYMENT")).nodes
            when params.has_key?(:provider_id)
              find_key_cap(Provider,params[:provider_id], cap("READ","PROVIDER")).nodes
            else
              model.all
            end.visible(cap("READ"),@current_user.id)
    respond_to do |format|
      format.html { @list.to_a.delete_if { |n| n.system } }
      format.json { render api_index model, @list }
    end
  end

  # API /api/status/nodes(/:id)
  def status
    status = {}
    nodes = nil
    Node.transaction do   # performance optimization
      nodes = if params[:id]
                visible(model,cap("READ")).where(id: params[:id])
              else
                visible(model, cap("READ")).non_system
              end
    end
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
    render api_array status.to_json
  end

  def show
    @node = find_key_cap(model, params[:id], cap("READ"))
    respond_to do |format|
      format.html {  } # show.html.erb
      format.json { render api_show @node }
    end
  end

  # Get the addresses allocated to a node on a network.
  def addresses
    if params[:network]
      res = {}
      model.transaction do
        @node = find_key_cap(model, params[:node_id], cap("READ"))
        @net = find_key_cap(Network, params[:network], cap("READ","NETWORK"))
        res = {
          "node" => @node.name,
          "network" => @net.name,
          "category" => @net.category,
          "addresses" => @net.node_allocations(@node).map{|a|a.to_s}
        }
      end
      render :json => res, :content_type=>cb_content_type(:addresses, "object")
      return
    end
    res = []
    model.transaction do
      nets = if params[:category]
               visible(Network,cap("READ","NETWORK")).in_category(params[:category])
             else
               visible(Network,cap("READ","NETWORK"))
             end
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
    end
    render :json => res, :content_type=>cb_content_type(:addresses, "array")
  end

  # RESTful DELETE of the node resource
  def destroy
    if params[:group_id]
      model.transaction do
        # Arguably, these should both be UPDATE
        @node = find_key_cap(model, params[:id] || params[:name], cap("READ"))
        @group = find_key_cap(Group, params[:group_id], cap("DESTROY", "GROUP"))
        @group.nodes.delete(@node) if @group.nodes.include?(@node)
      end
      render :text=>I18n.t('api.removed', :item=>'node', :collection=>'group')
      return
    end
    model.transaction do
      @node = find_key_cap(model,params[:id] || params[:name], cap("DESTROY"))
      @node.destroy
    end
    respond_to do |format|
      format.html { redirect_to deployment_path(@node.deployment_id) }
      format.json { render api_delete @node }
    end
  end

  def power
    if request.get?
      @node = find_key_cap(model,params[:id] || params[:name] || params[:node_id],cap("READ"))
      render api_array @node.power.keys
    elsif request.put?
      @node = find_key_cap(model,params[:id] || params[:name] || params[:node_id],cap("POWER"))
      params.require(:poweraction)
      @poweraction = params[:poweraction].to_sym
      if @node.power.include? @poweraction
        result = @node.power.send(@poweraction) rescue nil
        render api_not_implemented(@node, @poweraction, "see logs for internal error") if result.nil?
        render api_result({"id" => @node.id, "action" => @poweraction, "result" => result })
      else
        render api_not_implemented @node, @poweraction, @node.power.keys
      end
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

  def create
    if params[:group_id] and params[:node_id]
      g = nil
      Group.transaction do
        # Both of these should arguably be UPDATE
        g = find_key_cap(Group, params[:group_id], cap("UPDATE","GROUP"))
        n = find_key_cap(model, params[:node_id], cap("READ"))
        n.groups << g
      end
      render :text=>I18n.t('api.added', :item=>g.name, :collection=>'node.groups')
      return
    end
    default_net = nil
    Node.transaction do
      # Make sure the deployment and provder are in a tenant
      # we have NODE_CREATE capabilities for.
      depl = find_key_cap(Deployment,
                          params[:deployment] ||
                          params[:deployment_id] ||
                          "system",
                          cap("UPDATE","DEPLOYMENT"))
      provider = find_key_cap(Provider,
                              params[:provider] ||
                              params[:provider_id] ||
                              "metal",
                              cap("READ","PROVIDER"))
      params[:deployment_id] = depl.id
      params[:provider_id] = provider.id
      params.require(:name)
      params.require(:deployment_id)
      params.require(:provider_id)
      params[:tenant_id] ||= @current_user.current_tenant_id
      params[:variant] ||= provider.variant_default
      params[:profiles] ||= []
      validate_create(params[:tenant_id])
      hints = params[:hints] || {}
      Rails.logger.info("Node create params: #{params.inspect}")
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
                                         :os_family,
                                         :profiles))
      # Keep suport for mac and ip hints in short form around for legacy Sledgehammer purposes
      # This kinda-sorta bypasses network cap checking, but it is just too useful.
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
    # This winds up calling NodeRole.safe_create, which has to run
    # outside a transaction
    default_net.make_node_role(@node) if default_net
    render api_show @node
  end

  def update
    Node.transaction do
      @node = find_key_cap(model,params[:id],cap("UPDATE")).lock!
      ## Add better handling of deployment and tenant changing
      params[:deployment_id] = Deployment.find_key(params[:deployment]).id if params.has_key? :deployment
      params[:tenant_id] = Tenant.find_key(params[:tenant]).id if params.has_key? :tenant
      if request.patch?
        patch(@node,%w{name description target_role_id deployment_id allocated available alive bootenv tenant_id profiles})
      else
        simple_update(@node,%w{name description target_role_id deployment_id allocated available alive bootenv tenant_id profiles})
        # UGLY UGLY HACK because profiles are not being updated by simple_update (but it works in patch)
        if params.has_key? :profiles
          @node.profiles = params[:profiles]
          @node.save!
        end
      end
      validate_update_for(@node)
    end
    render api_show @node
  end

  #test_ methods support test functions that are not considered stable APIs
  # Do we still need this?
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
    @node = find_key_cap(model,params[:id] || params[:name] || params[:node_id],cap(action))
    @node.send(meth)
    render api_show @node
  end

end
