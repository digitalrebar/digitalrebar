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
    @filters = visible(model,cap("READ")).order('priority ASC')
    respond_to do |format|
      format.html {}
      format.json { render api_index model, @filters }
    end
  end

  def show
    @filter = find_key_cap(model, params[:id], cap("READ"))
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
    params[:tenant_id] ||= @current_user.current_tenant_id
    DnsNameFilter.transaction do
      validate_create(params[:tenant_id])
      @filter = DnsNameFilter.create! params.permit(:name, :matcher, :priority, :service, :template, :tenant_id)
    end

    respond_to do |format|
      format.html { redirect_to :action=>:index }
      format.json { render api_show @filter }
    end

  end

  def update
    model.transaction do
      @filter = find_key_cap(model, params[:id], cap("UPDATE"))
      @filter.update_attributes!(params.permit(:name, :matcher, :priority, :service, :template, :tenant_id))
    end
    respond_to do |format|
      format.html { render :action=>:show }
      format.json { render api_show @filter }
    end
  end

  def destroy
    model.transaction do
      @filter = find_key_cap(model, params[:id], cap("DESTROY"))
      @filter.destroy
    end
    respond_to do |format|
      format.html { redirect_to dns_name_filters_path() }
      format.json { render api_delete @filter }
    end
  end

  ## Again, why this method?
  def edit
    @dnf = find_key_cap(model, params[:id], cap("UPDATE"))
    respond_to do |format|
      format.html {  }
    end
  end

end
