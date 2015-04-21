# Copyright 2015, Greg Althaus
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
class NetworkRoutersController < ::ApplicationController
  respond_to :json

  def index
    @list = if params.has_key? :network_id or params.has_key? :network
              network =  Network.find_key params[:network_id] || params[:network]
              (network.router ? [network.router] : [])
            else
              NetworkRouter.all
            end
    respond_to do |format|
      format.json { render api_index NetworkRouter, @list }
      format.html { }
    end
  end

  def show
    if params[:network_id] || params[:network] 
      network = Network.find_key (params[:network_id] || params[:network])
      # since there is only 1 router per network, it does not matter what was actually asked for
      @item = network.router 
    else
      @item = NetworkRouter.find_key params[:id]
    end
    respond_to do |format|
      format.json { render api_show @item }
      format.html { }
    end
  end

  def destroy
    if params[:network_id] || params[:network] 
      network = Network.find_key (params[:network_id] || params[:network])
      if network.router
        @network_router = network.router
      else
        raise "CANNOT DELETE: no router on network #{params[:network_id]}"
      end
    else
      @network_router = NetworkRouter.find_key(params[:id])
    end
    @network_router.destroy
    render api_delete @network_router
  end

  def create
    params[:network_id] = Network.find_key(params[:network]).id if params.has_key? :network
    params.require(:network_id)
    params.require(:address)
    @router =  NetworkRouter.create! params.permit(:network_id,:address,:pref)
    render api_show @router
  end

  def update
    if params[:network_id] || params[:network] 
      network = Network.find_key (params[:network_id] || params[:network])
      if network.router
        @network_router = network.router
      else
        raise "CANNOT UPDATE: no router on network #{params[:network_id]}"
      end
    else
      @network_router = NetworkRouter.find_key params[:id]
    end
    @network_router.update_attributes!(params.permit(:address,:pref))
    render api_show @network_router
  end


end

