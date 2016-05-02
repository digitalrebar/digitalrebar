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
    @list = if params.has_key?(:node_id)
      Node.find_key(params[:node_id]).providers
    else
      Provider.all
    end
    respond_to do |format|
      format.html {  }
      format.json { render api_index Provider, @list }
    end
  end

  def show
    @item = if params[:id] == 'create'
      t = params[:new][:type]
      Provider.new(name: t.downcase, id: -1, type: t, description: I18n.t('not_set'))
    else
      Provider.find_key(params[:id])
    end
    respond_to do |format|
      format.html { render :show  }
      format.json { render api_show @item }
    end
  end

  def update
    hashfix if params[:auth_details].is_a? Hash # address UI formatting
    Provider.transaction do
      @item = Provider.find_key(params[:id]).lock!
      if request.patch?
        patch(@item,%w{name item type description auth_details})
      else
        @item.update_attributes!(params.permit(:name, :description, :type, :auth_details))
      end
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
    @item = Provider.create!(name: params[:name],
                                 type: params[:type],
                                 description: params[:description],
                                 auth_details: params[:auth_details])
    respond_to do |format|
      format.html { 
        flash[:notice] = @item.name + " " + I18n.t('save')
        redirect_to provider_path(@item.id) 
      }
      format.json { render api_show @item }
    end
  end

  def destroy
    @item = Provider.find_key(params[:id])
    @item.destroy
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
      elsif value.start_with? "!"
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
