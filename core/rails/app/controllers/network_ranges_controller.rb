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
class NetworkRangesController < ::ApplicationController
  self.model = NetworkRange
  self.cap_base = "NETWORK"
 
  def index
    model.transaction do
      @list = if params.has_key? :network_id or params.has_key? :network
                find_key_cap(Network,params[:network_id] || params[:network],cap("READ")).
                  network_ranges
              else
                visible(model,cap("READ"))
              end
    end
    respond_to do |format|
      format.html { }
      format.json { render api_index NetworkRange, @list }
    end
  end

  def show
    @range = if params[:network_id]
               find_key_cap(Network, params[:network_id], cap("READ")).
                 network_ranges.find_key(params[:id])
             else
               find_key_cap(model, params[:id], cap("READ"))
             end
    respond_to do |format|
      format.html {
        # Why?
        @list = [@range]
        render :action=>:index
      }
      format.json { render api_show @range }
    end
  end

  def create
    params[:overlap] = false unless params.key?(:overlap)
    params.require(:name)
    params.require(:first)
    params.require(:last)
    # Legacy support for special "host" and "dhcp" range names
    case params[:name]
    when "host"
      params[:allow_anon_leases] = false
      params[:allow_bound_leases] = true
    when "dhcp"
      params[:allow_anon_leases] = true
      params[:allow_bound_leases] = false
    end
    if params[:anon_lease_time] && params[:anon_lease_time] == 0
      params[:anon_lease_time] = 60
    end
    if params[:bound_lease_time] && params[:bound_lease_time] == 0
      params[:bound_lease_time] = 2592000
    end
    model.transaction do
      net = find_key_cap(Network, params[:network] || params[:network_id], cap("UPDATE"))
      params[:network_id] = net.id
      # There is a case to be made for making this default to the network tenant ID instead.
      params[:tenant_id] ||= @current_user.current_tenant_id
      validate_create(params[:tenant_id])
      @range =  NetworkRange.create! params.permit(:name,
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
                                                   :use_team,
                                                   :anon_lease_time,
                                                   :bound_lease_time,
                                                   :allow_anon_leases,
                                                   :allow_bound_leases)
    end
    render api_show @range
  end

  def update
    NetworkRange.transaction do
      params[:network_id] = Network.find_key(params[:network]).id if params.has_key? :network
      @network_range = if params.has_key? :id
                         find_key_cap(model, params[:id], cap("UPDATE")).lock!
                       else
                         net = find_key_cap(Network,
                                            params[:network] || params[:network_id],
                                            cap("READ"))
                         visible(model, cap("UPDATE")).
                           find_by!(name: params[:name], network_id: net.id).lock!
                       end
      simple_update(@network_range,%w{name first last conduit vlan team_mode overlap use_vlan use_bridge use_team tenant_id})
    end
    render api_show @network_range
  end

  # only works with ID, not name!
  def destroy
    model.transaction do
      @range = find_key_cap(model, params[:id], cap("DESTROY"))
      if params[:network_id]
        raise "Range is not from the correct Network" unless @range.network_id = params[:network_id]
      end
      @range.destroy
    end
    render api_delete @range
  end

end
