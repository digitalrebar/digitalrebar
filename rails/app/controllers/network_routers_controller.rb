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
  self.model = NetworkRouter
  self.cap_base = "NETWORK"

  def match
    attrs = NetworkRouter.attribute_names.map{|a|a.to_sym}
    objs = []
    ok_params = params.permit(attrs)
    objs = validate_match(ok_params, :tenant_id, "NETWORK", NetworkRouter)
    respond_to do |format|
      format.html {}
      format.json { render api_index NetworkRouter, objs }
    end
  end
  
  def index
    @list = if params.has_key? :network_id or params.has_key? :network
              network =  Network.find_key params[:network_id] || params[:network]
              (network.router ? [network.router] : [])
            else
              NetworkRouter.all.to_a
            end
    t_ids = build_tenant_list("NETWORK_READ")
    @list.delete_if { |x| !t_ids.include? x.tenant_id }
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
    validate_read(@item.tenant_id, "NETWORK", NetworkRouter, @item.id)
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
    validate_destroy(@network_router.tenant_id, "NETWORK", NetworkRouter, @network_router.id)
    @network_router.destroy
    render api_delete @network_router
  end

  def create
    network = Network.find_key(params[:network] || params[:network_id])
    params[:network_id] = network.id
    params.require(:network_id)
    params.require(:address)
    unless params[:tenant_id]
      params[:tenant_id] = @current_user.current_tenant_id
    end
    # cannot create if existing 
    if network.router 
      render api_conflict network.router
    else
      validate_create(@network.tenant_id, "NETWORK", NetworkRouter)
      @router =  NetworkRouter.create! params.permit(:network_id,:address,:pref,:tenant_id)
      render api_show @router
    end
  end

  def update
    NetworkRouter.transaction do
      if params[:network_id] || params[:network] 
        network = Network.find_key(params[:network_id] || params[:network])
        if network.router
          @network_router = network.router.lock!
        else
          raise "CANNOT UPDATE: no router on network #{params[:network_id]}"
        end
      else
        @network_router = NetworkRouter.find_key(params[:id]).lock!
      end
      validate_update(@network_router.tenant_id, "NETWORK", NetworkRouter, @network_router.id)
      if request.patch?
        patch(@network_router,%w{address pref tenant_id})
      else
        @network_router.update_attributes!(params.permit(:address,:pref,:tenant_id))
      end
    end
    render api_show @network_router
  end


end

