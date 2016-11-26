# Copyright 2013, Dell 
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

class BarclampsController < ApplicationController
  self.model = Barclamp
  self.cap_base = "BARCLAMP"

  def index
    @list = visible(model, cap("READ"))
    respond_to do |format|
      format.html { }
      format.json { render api_index model, @list }
    end
  end

  def show
    @barclamp = find_key_cap(model, params[:id], cap("READ"))
    respond_to do |format|
      format.html {  }
      format.json { render api_show @barclamp }
    end
  end

  def update
    params.require(:value)
    if request.patch?
      raise "PATCH update for barclamps not implemented!"
    end
    # Update and create are basically the same action for barclamps.
    # Not the proper way to deal, I know, but...
    model.transaction do
      validate_create
      @barclamp = Barclamp.import_or_update(params[:value], @current_user.current_tenant_id)
    end
    respond_to do |format|
      format.html {  }
      format.json { render api_show @barclamp }
    end
  end

  def destroy
    render api_not_supported 'delete', 'barclamp'
  end

  def create
    params.require(:value)
    model.transaction do
      validate_create
      bc = params[:value]
      unless bc.is_a?(Hash)
        bc = YAML::load(bc)
      end
      bc['barclamp']['source_path'] ||= 'API_uploaded'
      @barclamp = Barclamp.import_or_update(bc, @current_user.current_tenant_id)
    end
    respond_to do |format|
      format.html {  }
      format.json { render api_show @barclamp }
    end
  end

end

