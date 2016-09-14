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
  self.model = Provider
  self.cap_base = "PROVIDER"

  # API GET /api/v2/providers
  def index
    @list = if params.has_key?(:node_id)
              find_key_cap(Node,params[:node_id],cap("READ","NODE")).
                providers.visible(cap("READ"),@current_user.id)
            else
              visible(model, cap("READ"))
            end
    respond_to do |format|
      format.html {  }
      format.json { render api_index model, @list }
    end
  end

  def show
    @item = find_key_cap(model, params[:id], cap("READ"))
    respond_to do |format|
      format.html { render :show  }
      format.json { render api_show @item }
    end
  end

  def update
    hashfix if params[:auth_details].is_a? Hash # address UI formatting
    Provider.transaction do
      @item = find_key_cap(model,params[:id],cap("UPDATE")).lock!
      simple_update(@item,%w{name item type description auth_details tenant_id})
    end
    respond_to do |format|
      format.html { 
        flash[:notice] = @item.name + " " + I18n.t('save')
        render :show 
      }
      format.json { render api_show @item }
    end
  end

  def create
    hashfix if params[:auth_details].is_a? Hash # address UI formatting
    # UI passes array, clean that up
    params[:type] = params[:type].first if params[:type].kind_of?(Array)
    params.require(:name)
    params.require(:type)
    params.require(:auth_details)
    params[:tenant_id] ||= @current_user.current_tenant_id
    model.transaction do
      validate_create(params[:tenant_id])
      @item = Provider.create!(name: params[:name],
                               type: params[:type],
			       tenant_id: params[:tenant_id],
                               description: params[:description],
                               auth_details: params[:auth_details])
    end
    respond_to do |format|
      format.html { 
        flash[:notice] = @item.name + " " + I18n.t('save')
        redirect_to provider_path(@item.id) 
      }
      format.json { render api_show @item }
    end
  end

  def destroy
    model.transaction do
      @item = find_key_cap(model,params[:id],cap("DESTROY"))
      @item.destroy
    end
    render api_delete @item
  end

  # provide the JSON expected by available providers
  def templates
    render api_array({ "AwsProvider" => AwsProvider.template, 
             "GoogleProvider" => GoogleProvider.template, 
             "OpenStackProvider" => OpenStackProvider.template,
             "PacketProvider" => PacketProvider.template
            })
  end

  private

  # address UI formatting
  def hashfix
    # deal w/ non-json form data from UI
    # if we're passing JSON then we need to convert that to a nested hash (ASSUME json params have json in the title)
    out = {}
    params[:auth_details].each do |key, value|
      if (value.is_a? String) and (key =~ /json/ or value.start_with?("{"))
        out[key] = JSON.parse(value) 
      elsif value.is_a? String and value.start_with? "!"
        # noop, we are going to drop values that start with !
      else
        out[key] = value
      end
    end
    # turn the hash into json for the provider
    params[:auth_details] = out.to_json
    Rails.logger.debug("Provider create/update auth_details saved as #{params[:auth_details]}")
  end

end
