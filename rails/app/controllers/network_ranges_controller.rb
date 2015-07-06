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

  def match
    attrs = NetworkRange.attribute_names.map{|a|a.to_sym}
    objs = NetworkRange.where(params.permit(attrs))
    respond_to do |format|
      format.html {}
      format.json { render api_index NetworkRange, objs }
    end
  end
  
  def index
    @list = if params.has_key? :network_id or params.has_key? :network
              network =  Network.find_key params[:network_id] || params[:network]
              network.network_ranges
            else
              NetworkRange.all
            end
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
    @range =  NetworkRange.create! params.permit(:name,
                                                 :network_id,
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
    if params.has_key? :id
      @network_range = NetworkRange.find_key params[:id]
    else
      @network_range = NetworkRange.where(:name=>params[:name], :network_id=>params[:network_id]).first
    end
    @network_range.update_attributes!(params.permit(:name,
                                                    :first,
                                                    :last,
                                                    :conduit,
                                                    :vlan,
                                                    :team_mode,
                                                    :overlap,
                                                    :use_vlan,
                                                    :use_bridge,
                                                    :use_team))
    render api_show @network_range
  end

end
