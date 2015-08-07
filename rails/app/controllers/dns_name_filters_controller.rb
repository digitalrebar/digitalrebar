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
  respond_to :html, :json

  def match
    attrs = DnsNameFilter.attribute_names.map{|a|a.to_sym}
    objs = []
    ok_params = params.permit(attrs)
    objs = DnsNameFilter.where(ok_params) if !ok_params.empty?
    respond_to do |format|
      format.html {}
      format.json { render api_index DnsNameFilter, objs }
    end
  end
  
  def show
    @filter = DnsNameFilter.find_key params[:id]
    respond_to do |format|
      format.html { }
      format.json { render api_show @filter }
    end
  end

  def index
    @filters = DnsNameFilter.order('priority ASC')
    respond_to do |format|
      format.html {}
      format.json { render api_index DnsNameFilter, @filters }
    end
  end

  def create
    params.require(:matcher)
    params.require(:priority)
    params.require(:service)
    params.require(:template)
    params.require(:name)
    DnsNameFilter.transaction do
      @filter = DnsNameFilter.create! params.permit(:name, :matcher, :priority, :service, :template)
    end

    respond_to do |format|
      format.html { redirect_to :action=>:index }
      format.json { render api_show @filter }
    end

  end

  def update
    @filter = DnsNameFilter.find_key(params[:id])

    @filter.update_attributes!(params.permit(:name, :matcher, :priority, :service, :template))
    respond_to do |format|
      format.html { render :action=>:show }
      format.json { render api_show @filter }
    end
  end

  def destroy
    @filter = DnsNameFilter.find_key(params[:id])
    @filter.destroy
    respond_to do |format|
      format.html { redirect_to dns_name_filters_path() }
      format.json { render api_delete @filter }
    end
  end

  def edit
    @dnf = DnsNameFilter.find_key params[:id]
    respond_to do |format|
      format.html {  }
    end
  end

end
