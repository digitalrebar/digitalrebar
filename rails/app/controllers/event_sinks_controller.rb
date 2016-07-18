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
    t_ids = build_tenant_list("EVENT_SINK_READ")
    @event_sinks = EventSink.where(tenant_id: t_ids)
    respond_to do |format|
      format.html { } 
      format.json { render api_index EventSink, @event_sinks }
    end
  end

  def show
    @event_sink = EventSink.find_key(params[:id])
    validate_read(@event_sink.tenant_id, "EVENT_SINK", EventSink, params[:id])
    respond_to do |format|
      format.html {  }
      format.json { render api_show @event_sink }
    end
  end

  def update
    EventSink.transaction do
      @event_sink = event.find_key(params[:id]).lock!
      validate_update(@event_sink.tenant_id, "EVENT_SINK", EventSink, params[:id])
      if request.patch?
        patch(@event_sink,%w{endpoint username authenticator notes tenant_id})
      else
        @event_sink.update_attributes!(params.permit(:endpoint,
                                                :username,
					        :tenant_id,
                                                :authenticator,
                                                :notes))
      end
    end
    render api_show @event_sink
  end

  def create
    params.require(:endpoint)
    unless params[:tenant_id]
      params[:tenant_id] = @current_user.current_tenant_id
    end
    validate_create(params[:tenant_id], "EVENT_SINK", EventSink)
    EventSink.transaction do
      @event_sink = EventSink.create!(params.permit(:endpoint,
                                               :username,
					       :tenant_id,
                                               :authenticator,
                                               :notes))
    end
    render api_show @event_sink
  end

  def destroy
    @es = EventSink.find_key(params[:id])
    validate_destroy(@es.tenant_id, "EVENT_SINK", EventSink, params[:id])
    @es.destroy
    render api_delete @es
  end


end
