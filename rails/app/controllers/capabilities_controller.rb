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
class CapabilitiesController < ::ApplicationController
  respond_to :html, :json

  def sample
    render api_sample(Capability)
  end

  def match
    attrs = Capability.attribute_names.map{|a|a.to_sym}
    objs = []
    ok_params = params.permit(attrs)
    objs = Capability.where(ok_params) if !ok_params.empty?
    respond_to do |format|
      format.html {}
      format.json { render api_index Capability, objs }
    end
  end

  def show
    @capability = Capability.find_key params[:id]
    respond_to do |format|
      format.html { }
      format.json { render api_show @capability }
    end
  end
  
  def index
    @capabilities = Capability.all
    respond_to do |format|
      format.html {}
      format.json { render api_index Capability, @capabilites }
    end
  end

  def create
    Capability.transaction do
      @capability = Capability.create! params.permit(:name,
                                             :description,
                                             :source)
    end

    respond_to do |format|
      format.html { redirect_to :action=>:index }
      format.json { render api_show @capability }
    end
  end

  def update
    Capability.transaction do
      @capability = Capability.find_key(params[:id]).lock!
      if request.patch?
        patch(@capability,%w{description name source})
      else
        @capability.update_attributes!(params.permit(:description, :name, :source))
      end
    end
    respond_to do |format|
      format.html { render :action=>:show }
      format.json { render api_show @capability }
    end
  end

  def destroy
    @capability = Capability.find_key(params[:id])
    @capability.destroy
    render api_delete @capability
  end

  def edit
    @capability = Capability.find_key params[:id]
    respond_to do |format|
      format.html {  }
    end
  end

end
