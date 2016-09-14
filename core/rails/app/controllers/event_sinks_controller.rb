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
class EventSinksController < ApplicationController
  self.model = EventSink
  self.cap_base = "EVENT_SINK"

  def index
    @event_sinks = visible(model, cap("READ"))
    respond_to do |format|
      format.html { } 
      format.json { render api_index model, @event_sinks }
    end
  end

  def show
    @event_sink = find_key_cap(model, params[:id], cap("READ"))
    respond_to do |format|
      format.html {  }
      format.json { render api_show @event_sink }
    end
  end

  def update
    EventSink.transaction do
      @event_sink = find_key_cap(model, params[:id], cap("UPDATE")).lock!
      simple_update(@event_sink,%w{endpoint username authenticator notes tenant_id})
    end
    render api_show @event_sink
  end

  def create
    params.require(:endpoint)
    params[:tenant_id] ||= @current_user.current_tenant_id
    EventSink.transaction do
      validate_create(params[:tenant_id])
      @event_sink = EventSink.create!(params.permit(:endpoint,
                                                    :username,
					            :tenant_id,
                                                    :authenticator,
                                                    :notes))
    end
    render api_show @event_sink
  end

  def destroy
    model.transaction do
      @es = find_key_cap(model,params[:id],cap("DESTROY"))
      @es.destroy
    end
    render api_delete @es
  end


end
