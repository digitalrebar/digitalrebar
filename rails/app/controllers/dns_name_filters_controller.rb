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

class DnsNameFiltersController < ::ApplicationController
  self.model = DnsNameFilter
  self.cap_base = "NETWORK"

  def index
    t_ids = build_tenant_list("NETWORK_READ")
    @filters = DnsNameFilter.where(tenant_id: t_ids).order('priority ASC')
    respond_to do |format|
      format.html {}
      format.json { render api_index DnsNameFilter, @filters }
    end
  end

  def show
    @filter = DnsNameFilter.find_key params[:id]
    validate_read(@filter.tenant_id, "NETWORK", DnsNameFilter, params[:id])
    respond_to do |format|
      format.html { }
      format.json { render api_show @filter }
    end
  end

  def create
    params.require(:matcher)
    params.require(:priority)
    params.require(:service)
    params.require(:template)
    params.require(:name)
    unless params[:tenant_id]
      params[:tenant_id] = @current_user.current_tenant_id
    end
    validate_create(params[:tenant_id], "NETWORK", DnsNameFilter)
    DnsNameFilter.transaction do
      @filter = DnsNameFilter.create! params.permit(:name, :matcher, :priority, :service, :template, :tenant_id)
    end

    respond_to do |format|
      format.html { redirect_to :action=>:index }
      format.json { render api_show @filter }
    end

  end

  def update
    @filter = DnsNameFilter.find_key(params[:id])
    validate_update(@filter.tenant_id, "NETWORK", DnsNameFilter, params[:id])
    @filter.update_attributes!(params.permit(:name, :matcher, :priority, :service, :template, :tenant_id))
    respond_to do |format|
      format.html { render :action=>:show }
      format.json { render api_show @filter }
    end
  end

  def destroy
    @filter = DnsNameFilter.find_key(params[:id])
    validate_destroy(@filter.tenant_id, "NETWORK", DnsNameFilter, params[:id])
    @filter.destroy
    respond_to do |format|
      format.html { redirect_to dns_name_filters_path() }
      format.json { render api_delete @filter }
    end
  end

  def edit
    @dnf = DnsNameFilter.find_key params[:id]
    validate_update(@dnf.tenant_id, "NETWORK", DnsNameFilter, params[:id])
    respond_to do |format|
      format.html {  }
    end
  end

end
