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
  self.model = Tenant
  self.cap_base = "TENANT"

  def index
    @tenants = visible(model, cap("READ"))
    respond_to do |format|
      format.html {}
      format.json { render api_index Tenant, @tenants }
    end
  end

  def show
    @tenant = find_key_cap(model, params[:id], cap("READ"))
    respond_to do |format|
      format.html { }
      format.json { render api_show @tenant }
    end
  end
  
  def create
    params[:parent_id] ||= @current_user.current_tenant_id
    validate_create(params[:parent_id])
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
    Tenant.transaction do
      @tenant = find_key_cap(model,params[:id],cap("UPDATE")).lock!
      # All sorts of room for mischief here.
      simple_update(@tenant,%w{description name parent_id})
      if @tenant.previous_changes["parent_id"] && ! capable(@tenant.parent_id,cap("UPDATE"))
        raise RebarForbiddenError.new(@tenant, model)
      end
    end
    respond_to do |format|
      format.html { render :action=>:show }
      format.json { render api_show @tenant }
    end
  end

  def destroy
    @tenant = find_key_cap(model, params[:id], cap("DESTROY"))
    @tenant.destroy
    render api_delete @tenant
  end

  ## Why is this here?
  def edit
    @tenant = find_key_cap(model, params[:id], cap("UPDATE"))
    respond_to do |format|
      format.html {  }
    end
  end

end
