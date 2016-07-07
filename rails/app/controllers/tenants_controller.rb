# Copyright 2016, RackN
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
class TenantsController < ::ApplicationController
  respond_to :html, :json

  def sample
    render api_sample(Tenant)
  end

  def match
    attrs = Tenant.attribute_names.map{|a|a.to_sym}
    objs = []
    ok_params = params.permit(attrs)
    objs = Tenant.where(ok_params) if !ok_params.empty?
    objs = [] unless validate_capability(@tenant.id, "TENANT_READ") # GREG: Really filter
    respond_to do |format|
      format.html {}
      format.json { render api_index Tenant, objs }
    end
  end

  def show
    @tenant = Tenant.find_key params[:id]
    @tenant = nil unless validate_tenant(@tenant.id, "TENANT_READ")
    unless @tenant
      raise "GREG: Make nicer: Should be 404"
    end
    respond_to do |format|
      format.html { }
      format.json { render api_show @tenant }
    end
  end
  
  def index
    @tenants = Tenant.all
    @tenants = [] unless validate_capability(@tenant.id, "TENANT_READ") # GREG: Really filter
    respond_to do |format|
      format.html {}
      format.json { render api_index Tenant, @tenants }
    end
  end

  def create
    unless validate_tenant(@tenant.id, "TENANT_CREATE")
      raise "GREG: Make nicer: Should be 409"
    end
    unless params[:parent_id]
      params[:parent_id] = @current_user.tenant_id
    end
    Tenant.transaction do
      @tenant = Tenant.create! params.permit(:name,
                                             :description,
                                             :parent_id)
    end

    respond_to do |format|
      format.html { redirect_to :action=>:index }
      format.json { render api_show @tenant }
    end
  end

  def update
    unless validate_tenant(@tenant.id, "TENANT_UPDATE")
      raise "GREG: Make nicer: Should be 409"
    end
    Tenant.transaction do
      @tenant = Tenant.find_key(params[:id]).lock!
      if request.patch?
        patch(@tenant,%w{description name parent_id})
      else
        @tenant.update_attributes!(params.permit(:description, :name, :parent_id))
      end
    end
    respond_to do |format|
      format.html { render :action=>:show }
      format.json { render api_show @tenant }
    end
  end

  def destroy
    unless validate_tenant(@tenant.id, "TENANT_DESTROY")
      raise "GREG: Make nicer: Should be 409"
    end
    @tenant = Tenant.find_key(params[:id])
    @tenant.destroy
    render api_delete @tenant
  end

  def edit
    unless validate_tenant(@tenant.id, "TENANT_UPDATE")
      raise "GREG: Make nicer: Should be 409"
    end
    @tenant = Tenant.find_key params[:id]
    respond_to do |format|
      format.html {  }
    end
  end

end
