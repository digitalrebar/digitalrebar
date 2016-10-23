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
#
class ProfilesController < ApplicationController
  self.model = Profile
  self.cap_base = "PROFILE"

  def index
    @profiles = model.order('name')
    respond_to do |format|
      format.html { } # show.html.erb
      format.json { render api_index model, @profiles }
    end
  end

  def show
    @profile = model.find_key params[:id]
    respond_to do |format|
      format.html {  }
      format.json { render api_show @profile }
    end
  end

  def update
    Profile.transaction do
      @profile= find_key_cap(model,params[:id],cap("UPDATE")).lock!
      simple_update(@profile,%w{values tenant_id})
    end
    render api_show @profile
  end

  def create
    params.require(:name)
    params.require(:values)
    params[:tenant_id] ||= @current_user.current_tenant_id
    Profile.transaction do
      validate_create(params[:tenant_id])
      @profile = Profile.create!(name: params[:name],
				 tenant_id: params[:tenant_id],
                                 values: params[:values])
    end
    render api_show @profile
  end

  def destroy
    model.transaction do
      @es = find_key_cap(model, params[:id],cap("DESTROY"))
      @es.destroy
    end
    render api_delete @es
  end
  
end
