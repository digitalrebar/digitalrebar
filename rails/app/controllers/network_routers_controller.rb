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
 
  def index
    @list = if params.has_key? :network_id or params.has_key? :network
              router = find_key_cap(Network,
                                    params[:network_id] || params[:network],
                                    cap("READ")).router
              router.nil? ? [] : [router]
            else
              visible(model,cap("READ"))
            end
    respond_to do |format|
      format.json { render api_index model , @list }
      format.html { }
    end
  end

  def show
    @item = if params[:network_id] || params[:network] 
              find_key_cap(Network, params[:network_id] || params[:network], cap("READ")).router
            else
              find_key_cap(model, params[:id], cap("READ"))
            end
    respond_to do |format|
      format.json { render api_show @item }
      format.html { }
    end
  end

  def destroy
    @router = if params[:network_id] || params[:network]
                net = find_key_cap(Network, params[:network_id] || params[:network], cap("UPDATE"))
                raise RebarForbiddenError("none",model) if net.router.nil?
                find_key_cap(model,net.router.id,cap("DESTROY"))
              else
                find_key_cap(model,params[:id],cap("DESTROY"))
              end
    @router.destroy
    render api_delete @router
  end

  def create
    network = find_key_cap(Network,params[:network] || params[:network_id], cap("UPDATE"))
    if network.router 
      render api_conflict network.router
      return
    end
    params.require(:network_id)
    params.require(:address)
    # There is a case to be made for letting the tenant default to the network tenant.
    params[:tenant_id] ||= @current_user.current_tenant_id
    validate_create(params[:tenant_id])
    @router =  NetworkRouter.create! params.permit(:network_id,:address,:pref,:tenant_id)
    render api_show @router
  end

  def update
    NetworkRouter.transaction do
      @router = if params[:network_id] || params[:network]
                  net = find_key_cap(Network, params[:network_id] || params[:network], cap("UPDATE"))
                  raise RebarForbiddenError("none",model) if net.router.nil?
                  find_key_cap(model,net.router.id,cap("UPDATE")).lock!
                else
                  find_key_cap(model,params[:id],cap("UPDATE")).lock!
                end
      simple_update(@network_router,%w{address pref tenant_id})
    end
    render api_show @network_router
  end

end
