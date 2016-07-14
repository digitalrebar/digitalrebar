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
  respond_to :json

  def sample
    render api_sample(NetworkRange)
  end

  def match
    attrs = NetworkRange.attribute_names.map{|a|a.to_sym}
    objs = []
    ok_params = params.permit(attrs)
    objs = validate_match(ok_params, :tenant_id, "NETWORK", NetworkRange)
    respond_to do |format|
      format.html {}
      format.json { render api_index NetworkRange, objs }
    end
  end
  
  def index
    @list = if params.has_key? :network_id or params.has_key? :network
              network =  Network.find_key params[:network_id] || params[:network]
              network.network_ranges.to_a
            else
              NetworkRange.all.to_a
            end
    t_ids = build_tenant_list("NETWORK_READ")
    @list.delete_if { |x| !t_ids.include? x.tenant_id }
    respond_to do |format|
      format.html { }
      format.json { render api_index NetworkRange, @list }
    end
  end

  def show
    if params[:network_id]
      network = Network.find_key params[:network_id]
      @range = network.network_ranges.find_key(params[:id])
    else
      @range = NetworkRange.find_key(params[:id])
    end
    validate_read(@range.tenant_id, "NETWORK", NetworkRange, params[:id])
    respond_to do |format|
      format.html {
                    @list = [@range]
                    render :action=>:index
                  }
      format.json { render api_show @range }
    end
  end

  def create
    params[:network_id] = Network.find_key(params[:network]).id if params.has_key? :network
    params[:overlap] = false unless params.key?(:overlap)
    params.require(:network_id)
    params.require(:name)
    params.require(:first)
    params.require(:last)
    unless params[:tenant_id]
      params[:tenant_id] = @current_user.current_tenant_id
    end
    validate_create(params[:tenant_id], "NETWORK", NetworkRange)
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
                                                 :use_team)
    render api_show @range
  end

  def update
    params[:network_id] = Network.find_key(params[:network]).id if params.has_key? :network
    NetworkRange.transaction do
      if params.has_key? :id
        @network_range = NetworkRange.find_key(params[:id]).lock!
      else
        @network_range = NetworkRange.find_by!(name: params[:name], network_id: params[:network_id]).lock!
      end
      validate_update(@network_range.tenant_id, "NETWORK", NetworkRange, @network_range.id)
      if request.patch?
        patch(@network_range,%w{name first last conduit vlan team_mode overlap use_vlan use_bridge use_team tenant_id})
      else
        @network_range.update_attributes!(params.permit(:name,
							:tenant_id,
                                                        :first,
                                                        :last,
                                                        :conduit,
                                                        :vlan,
                                                        :team_mode,
                                                        :overlap,
                                                        :use_vlan,
                                                        :use_bridge,
                                                        :use_team))
      end
    end
    render api_show @network_range
  end

  # only works with ID, not name!
  def destroy
    @range = NetworkRange.find params[:id]
    if params[:network_id]
      raise "Range is not from the correct Network" unless @range.network_id = params[:network_id]
    end
    validate_destroy(@range.tenant_id, "NETWORK", NetworkRange, params[:id])
    @range.destroy
    render api_delete @range
  end

end
