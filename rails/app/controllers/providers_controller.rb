# Copyright 2015, RackN
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
class ProvidersController < ApplicationController

  def sample
    render api_sample(Provider)
  end

  def match
    attrs = Provider.attribute_names.map{|a|a.to_sym}
    objs = []
    ok_params = params.permit(attrs)
    objs = Provider.where(ok_params) if !ok_params.empty?
    respond_to do |format|
      format.html {}
      format.json { render api_index Provider, objs }
    end
  end

  # API GET /api/v2/providers
  def index
    @providers = if params.has_key?(:node_id)
      Node.find_key(params[:node_id]).providers
    else
      Provider.all
    end
    respond_to do |format|
      format.html { }
      format.json { render api_index Provider, @providers }
    end
  end

  def show
    @provider = Provider.find_key(params[:id])
    respond_to do |format|
      format.html {  }
      format.json { render api_show @provider }
    end
  end

  def update
    Provider.transaction do
      @nm = Provider.find_key(params[:id]).lock!
      if request.patch?
        patch(@nm,%w{name type auth_details})
      else
        @nm.update_attributes!(params.permit(:name, :type, :auth_details))
      end
    end
    render api_show @nm
  end

  def create
    params.require(:name)
    params.require(:type)
    params.require(:auth_details)
    @provider = Provider.create!(name: params[:name],
                                 type: params[:type],
                                 auth_details: params[:auth_details])
    render api_show @provider
  end

  def destroy
    render api_delete Provider
  end

end
