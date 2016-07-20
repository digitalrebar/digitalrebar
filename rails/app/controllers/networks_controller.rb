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
  self.model = Network
  self.cap_base = "NETWORK"

  def auto_ranges
    @network = find_key_cap(model, params[:id], cap("READ"))
    @node = find_key_cap(Node, params[:node_id], cap("READ","NODE"))
    render api_index NetworkRange, @network.auto_ranges(@node)
  end

  def show
    @network = begin
                 lookup_by_address
               rescue
                 find_key_cap(model, params[:id],cap("READ"))
               end
    respond_to do |format|
      format.html { }
      format.json { render api_show @network }
    end
  end

  def index
    if params[:address]
      # Why do we have this weird non-list special case?
      net = lookup_by_address
      render api_show net
      return
    end
    @networks = if (params[:category])
                  visible(model,cap("READ")).in_category(params[:category])
                else
                  @networks = visible(model, cap("READ"))
                end
    respond_to do |format|
      format.html {}
      format.json { render api_index Network, @networks }
    end
  end

  def create
    # cleanup inputs
    depl = find_key_cap(Deployment,
                        params[:deployment] ||
                        params[:deployment_id] ||
                        "system",
                        cap("UPDATE","DEPLOYMENT"))
    params[:use_vlan] = true if !params.key?(:use_vlan) && params[:vlan].to_i > 0 rescue false
    params[:vlan] ||= 0
    params[:use_team] = true if !params.key?(:use_team) && params[:team_mode].to_i > 0 rescue false
    params[:team_mode] ||= 5
    params[:configure] = true unless params.key?(:configure)
    params[:deployment_id] = depl.id 
    params[:group] ||= "default"
    params.require(:category)
    params.require(:group)
    params.require(:conduit)
    params.require(:deployment_id)
    params.delete(:v6prefix) if params[:v6prefix] == "" or params[:v6prefix] == "none"
    params[:name] = "#{params[:category]}-#{params[:group]}"
    params[:tenant_id] ||= @current_user.current_tenant_id
    validate_create(params[:tenant_id])
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

  # Why?
  def map
    @networks = visible(model, cap("READ"))
    @nodes = visible(Node, cap("READ","NODE"))
  end

  # Allocations for a node in a network.
  # Includes the automatic IPv6 address.
  def allocations
    network = find_key_cap(model, params[:id],cap("READ"))
    node = find_key_cap(Node, params[:node],cap("READ","NODE"))
    render :json => network.node_allocations(node).map{|a|a.to_s}, :content_type=>cb_content_type(:allocations, "array")
  end

  def update
    Network.transaction do
      @network = find_key_cap(model, params[:id],cap("UPDATE")).lock!
      # Sorry, but no changing of the admin conduit for now.
      params.delete(:conduit) if @network.name == "admin"
      params.delete(:v6prefix) if params[:v6prefix] == ""
      # we need to reset the use_team flag if we only get a team mode
      if params[:team_mode]
        params[:use_team] = (params[:team_mode].to_i > 0) unless params[:use_team]
      end
      simple_update(@network,%w{description vlan use_vlan v6prefix use_bridge team_mode use_team conduit configure pbr category group deployment_id tenant_id})
    end
    respond_to do |format|
      format.html { render :action=>:show }
      format.json { render api_show @network }
    end
  end

  def destroy
    @network = find_key_cap(model,params[:id],cap("DESTROY"))
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
    network = find_key_cap(model, params[:id], cap("ALLOCATE"))
    node = find_key_cap(Node,params[:node_id],cap("UPDATE","NODE"))
    ret = if !params[:range]
            network.auto_allocate(node)
          else
            rnetwork.ranges.visible(cap("UPDATE"),@current_user.id).
              find_by!(name: params[:range]).allocate(node,params[:suggestion])
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

  # Why?
  def edit
    @network = find_key_cap(model, params[:id],cap("UPDATE"))
    respond_to do |format|
      format.html {  }
    end
  end

  private

  def lookup_by_address
    net = nil
    addr = IP.coerce(params[:id])
    visible(model,cap("READ")).
      where(category: params[:category] || "admin").select do |n|
      n.ranges.visible(cap("READ"),@current_user.id).each do |r|
        net = n if r === addr
        break if net
      end
      break if net
    end
    net
  end

end
