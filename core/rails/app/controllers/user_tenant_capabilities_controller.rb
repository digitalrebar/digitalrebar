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
class UserTenantCapabilitiesController < ::ApplicationController
  self.model = UserTenantCapability
  self.cap_base = "USER_TENANT_CAPABILITY"

  # Note that the visible helper ignores the cap here for now.
  # See it for the exact rules.
  def index
    model.transaction do
      @caps = visible(model,cap("READ"))
      @users = visible(User,cap("READ","USER")).
               where(id: @caps.to_a.map{|c|c.user_id}).distinct
      @tenants = visible(Tenant,cap("READ","TENANT")).
                 where(id: @caps.to_a.map{|c|c.tenant_id}).distinct
      @capabilities = visible(Capability,cap("READ","CAPABILITY")).
                      where(id: @caps.to_a.map{|c|c.capability_id}).distinct
    end
    respond_to do |format|
      format.html { }
      format.json { render api_index model, @caps }
    end
  end

  def show
    @cap = find_key_cap(model,params[:id],cap("READ"))
    respond_to do |format|
      format.html { }
      format.json { render api_show @cap }
    end
  end

  def create
    model.transaction do
      c = find_key_cap(Capability, params[:capability_id], cap("READ","CAPABILITY"))
      t = find_key_cap(Tenant, params[:tenant_id], cap("UPDATE","TENANT"))
      u = find_key_cap(User, params[:user_id], cap("UPDATE","USER"))
      # Why is this not named CREATE
      raise RebarForbiddenError.new("new", model) unless capable(t.id, cap("ADD"))
      @cap = model.create!(tenant_id: t.id,
                           user_id: u.id,
                           capability_id: c.id)
      if params.has_key?(:inheritable) && params[:inheritable] == false
        @cap.inheritable = false
        @cap.save!
      end
    end

    respond_to do |format|
      format.html { redirect_to :action=>:index }
      format.json { render api_show @cap }
    end
  end

  def update
    render api_not_supported 'put', 'user_tenant_capabilitiies/:id'
  end

  def destroy
    model.transaction do
      @cap = if params[:id]
               # This lies for now.  Once we have proper seperation between
               # READ, ADD (or hopefully CREATE), and DESTROY, it will
               # tell the truth
               find_key_cap(model, params[:id], cap("DESTROY"))
             else
               c = find_key_cap(Capability, params[:capability_id], cap("READ","CAPABILITY"))
               t = find_key_cap(Tenant, params[:tenant_id], cap("UPDATE","TENANT"))
               u = find_key_cap(User, params[:user_id], cap("UPDATE","USER"))
               model.find_by!(tenant_id: t.id, capability_id: c.id, user_id: u.id)
             end
      # Compensate for visible and find_key_cap not working
      raise RebarForbiddenError.new(@cap.id, model) unless capable(@cap.tenant_id, cap("DESTROY"))
      @cap.destroy
    end
    respond_to do |format|
      format.html { redirect_to :action=>:index }
      format.json { render api_delete @cap }
    end
  end

end
