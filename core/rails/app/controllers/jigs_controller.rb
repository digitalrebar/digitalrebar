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
# limitations under the License.1
#
#
class JigsController < ApplicationController
  self.model = Jig
  self.cap_base = "JIG"
 
  def index
    @jigs = visible(model,cap("READ"))
    respond_to do |format|
      format.html {  } # show.html.erb
      format.json { render api_index model, @jigs }
    end
  end

  def show
    respond_to do |format|
      @jig = find_key_cap(model, params[:id], cap("READ"))
      format.html {  }
      format.json { render api_show @jig }
    end
  end

  def create
    render api_not_supported("post", "jigs")
  end

  def update
    Jig.transaction do
      @jig = find_key_cap(model, params[:id], cap("UPDATE")).lock!
      simple_update(@jig,%w{description active server client_name key})
    end
    render api_show @jig
  end

  def destroy
    render api_not_supported("post", "jigs")
  end

  # PUT
  # calls jig.flush to clear temporary data (if jig supports it)
  def flush
    model.transaction do
      @jig = find_key_cap(model, params[:jig_id], cap("FLUSH"))
      @jig.flush
    end
    render api_show @jig    
  end

  def activate
    Jig.transaction do
      jig = find_key_cap(model, params[:jig_id], cap("UPDATE"))

      # if this is test, we remap all external roles to test
      if jig.name == 'test'
        Role.all.each do |r|
          unless Jig::INTERNAL.include? r.jig.name 
            r.jig = jig
            r.save!
          end
        end
      end
      jig.active = true
      jig.save!
    end
    render api_show jig
  end

end
