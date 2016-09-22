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
  self.model = Capability
  self.cap_base = "CAPABILITY"

  def show
    @capability = find_key_cap(model, params[:id], cap("READ"))
    respond_to do |format|
      format.html { }
      format.json { render api_show @capability }
    end
  end

  def index
    @capabilities = visible(model, cap("READ"))
    respond_to do |format|
      format.html {}
      format.json { render api_index model, @capabilities }
    end
  end

  def create
    Capability.transaction do
      validate_create
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
      @capability = find_key_cap(model, params[:id], cap("UPDATE")).lock!
      simple_update(@capability,%w{description source})
      # for select cap types, we allow edits
      if ["dr-groups","user-defined"].include? @capability.source
        @capability.includes = params[:includes]
        @capability.save!
      end
    end
    respond_to do |format|
      format.html { render :action=>:show }
      format.json { render api_show @capability }
    end
  end

  def destroy
    model.transaction do
      @capability = find_key_cap(model,params[:id], cap("DESTROY"))
      @capability.destroy
    end
    render api_delete @capability
  end

  ### Why do we have an edit method?
  def edit
    @capability = find_key_cap(model, params[:id], cap("UPDATE"))
    respond_to do |format|
      format.html {  }
    end
  end

end
