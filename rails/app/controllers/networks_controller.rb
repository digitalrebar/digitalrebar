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
class NetworksController < ::ApplicationController
  respond_to :html, :json

  add_help(:show,[:network_id],[:get])

  def sample
    render api_sample(Network)
  end

  def match
    attrs = Network.attribute_names.map{|a|a.to_sym}
    objs = []
    ok_params = params.permit(attrs)
    objs = validate_match(ok_params, :tenant_id, "NETWORK", Network)
    respond_to do |format|
      format.html {}
      format.json { render api_index Network, objs }
    end
  end

  def auto_ranges
    @network = Network.find_key params[:id]
    @node = Node.find_key params[:node_id]
    validate_read(@network.tenant_id, "NETWORK", Network, params[:id])
    validate_read(@node.tenant_id, "NODE", Node, params[:node_id])
    ranges = @network.auto_ranges(@node)
    render api_index NetworkRange, ranges
  end

  def show
    begin
      addr = IP.coerce(params[:id])
      @network = Network.lookup_network(addr, (params[:category] || "admin"))
    rescue 
      @network = Network.find_key params[:id]
    end
    validate_read(@network.tenant_id, "NETWORK", Network, @network.id)
    respond_to do |format|
      format.html { }
      format.json { render api_show @network }
    end
  end
  
  def index
    if (params[:address])
      @network = Network.lookup_network(params[:address], (params[:category] || "admin"))
      validate_read(@network.tenant_id, "NETWORK", Network, @network.id)
      render api_show @network
      return
    elsif (params[:category])
      @networks = Network.in_category(params[:category]).to_a
    else
      @networks = Network.all.to_a
    end
    t_ids = build_tenant_list("NETWORK_READ")
    @networks.delete_if { |x| !t_ids.include? x.tenant_id }
    respond_to do |format|
      format.html {}
      format.json { render api_index Network, @networks }
    end
  end

  def create

    # cleanup inputs
    params[:use_vlan] = true if !params.key?(:use_vlan) && params[:vlan].to_i > 0 rescue false
    params[:vlan] ||= 0
    params[:use_team] = true if !params.key?(:use_team) && params[:team_mode].to_i > 0 rescue false
    params[:team_mode] ||= 5
    params[:configure] = true unless params.key?(:configure)
    params[:deployment_id] = Deployment.find_key(params[:deployment]).id if params.has_key? :deployment
    params[:deployment_id] ||= 1
    params[:group] ||= "default"
    params.require(:category)
    params.require(:group)
    params.require(:conduit)
    params.require(:deployment_id)
    params.delete(:v6prefix) if params[:v6prefix] == "" or params[:v6prefix] == "none"
    params[:name] = "#{params[:category]}-#{params[:group]}"
    unless params[:tenant_id]
      params[:tenant_id] = @current_user.tenant_id
    end
    validate_create(params[:tenant_id], "NETWORK", Network)
    Network.transaction do
      @network = Network.create! params.permit(:name,
                                               :conduit,
                                               :description,
					       :tenant_id,
                                               :deployment_id,
                                               :vlan,
                                               :use_vlan,
                                               :use_bridge,
                                               :team_mode,
                                               :configure,
                                               :pbr,
                                               :category,
                                               :group,
                                               :use_team,
                                               :v6prefix)
      # make it easier to batch create ranges with a network
      if params.key? :ranges
        ranges = params[:ranges].is_a?(String) ? JSON.parse(params[:ranges]) : params[:ranges]
        ranges.each do |range|
          range[:network_id] = @network.id
          range[:overlap] = false unless range.key?(:overlap)
          range_params = ActionController::Parameters.new(range)
          range_params.require(:name)
          range_params.require(:network_id)
          range_params.require(:first)
          range_params.require(:last)
	  unless range_params[:tenant_id]
             range_params[:tenant_id] = @current_user.tenant_id
	  end
          NetworkRange.create! range_params.permit(:name,
                                                   :network_id,
						   :tenant_id,
                                                   :first,
                                                   :last,
                                                   :conduit,
                                                   :vlan,
                                                   :team_mode,
                                                   :overlap,
                                                   :use_vlan,
                                                   :use_bridge,
                                                   :use_team)
        end
        params.delete :ranges
      end

      # make it easier to batch create routers with a network
      if params.key? :router
        router = router.is_a?(String) ? JSON.parse(params[:router]) : params[:router]
        router[:network_id] = @network.id
        router_params = ActionController::Parameters.new(router)
        router_params.require(:network_id)
        router_params.require(:address)
        router_params.require(:pref)
        unless router_params[:tenant_id]
          router_params[:tenant_id] = @current_user.tenant_id
        end
        NetworkRouter.create! router_params.permit(:network_id,:address,:pref,:tenant_id)
        params.delete :router
      end
    end

    respond_to do |format|
      format.html { redirect_to :action=>:index }
      format.json { render api_show @network }
    end

  end

  def map
    t_ids = build_tenant_list("NETWORK_READ")
    @networks = Network.where(tenant_id: t_ids)
    t_ids = build_tenant_list("NODE_READ")
    @nodes = Node.where(tenant_id: t_ids).non_system.sort
  end

  # Allocations for a node in a network.
  # Includes the automatic IPv6 address.
  def allocations
    network = Network.find_key params[:id]
    raise "Must include a node parameter" unless params.key?(:node)
    nodename = params[:node]
    node = Node.find_key nodename
    validate_read(network.tenant_id, "NETWORK", Network, params[:id])
    validate_read(node.tenant_id, "NODE", Node, nodename)
    render :json => network.node_allocations(node).map{|a|a.to_s}, :content_type=>cb_content_type(:allocations, "array")
  end

  add_help(:update,[:id, :conduit, :team_mode, :use_team, :vlan, :use_vlan, :configure],[:put])
  def update
    Network.transaction do
      @network = Network.find_key(params[:id]).lock!
      # Sorry, but no changing of the admin conduit for now.
      params.delete(:conduit) if @network.name == "admin"
      params.delete(:v6prefix) if params[:v6prefix] == ""
      # we need to reset the use_team flag if we only get a team mode
      if params[:team_mode]
        params[:use_team] = (params[:team_mode].to_i > 0) unless params[:use_team]
      end
      validate_update(@network.tenant_id, "NETWORK", Network, params[:id])
      if request.patch?
        patch(@network,%w{description vlan use_vlan v6prefix use_bridge team_mode use_team conduit configure pbr category group deployment_id tenant_id})
      else
        @network.update_attributes!(params.permit(:description, :vlan, :use_vlan, :v6prefix, :use_bridge, :team_mode, :use_team, :conduit, :configure, :pbr, :category, :group, :deployment_id, :tenant_id))
      end
    end
    respond_to do |format|
      format.html { render :action=>:show }
      format.json { render api_show @network }
    end
  end

  def destroy
    @network = Network.find_key(params[:id])
    validate_destroy(@network.tenant_id, "NETWORK", Network, params[:id])
    @network.destroy
    render api_delete @network
  end

  def ip
    if request.get?
      allocations
    elsif request.post?
      allocate_ip
    elsif request.delete?
      deallocate_ip
    end
  end

  def allocate_ip
    network = Network.find_key(params[:id])
    node = Node.find_key(params[:node_id])
    validate_action(network.tenant_id, "NETWORK", Network, params[:network_id], "ALLOCATE")
    validate_update(node.tenant_id, "NODE", Node, params[:node_id])
    if ! params[:range]
      ret = network.auto_allocate(node)
    else
      range = network.ranges.where(:name => params[:range]).first
      suggestion = params[:suggestion]
      ret = range.allocate(node,suggestion)
    end
    render :json => ret
  end

  def deallocate_ip
    raise ArgumentError.new("Cannot deallocate addresses for now")
    node = Node.find_key(params[:node_id])
    allocation = NetworkAllocation.where(:address => params[:cidr], :node_id => node.id)

    validate_action(allocation.tenant_id, "NETWORK", NetworkAllocation, params[:cidr], "DEALLOCATE")
    validate_update(node.tenant_id, "NODE", Node, params[:node_id])
    allocation.destroy
  end

  def enable_interface
    raise ArgumentError.new("Cannot enable interfaces without IP address allocation for now.")

    deployment_id = params[:deployment_id]
    deployment_id = nil if deployment_id == "-1"
    network_id = params[:id]
    node_id = params[:node_id]

    ret = @barclamp.network_enable_interface(deployment_id, network_id, node_id)
    return render :text => ret[1], :status => ret[0] if ret[0] != 200
    render :json => ret[1]
  end

  def edit
    @network = Network.find_key params[:id]
    validate_update(@network.tenant_id, "NETWORK", Network, params[:id])
    respond_to do |format|
      format.html {  }
    end
  end

end
