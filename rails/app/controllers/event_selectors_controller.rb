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
class EventSelectorsController < ApplicationController
  self.model = EventSelector
  self.cap_base = "EVENT_SELECTOR"

  def index
    @event_selectors = if params.key(:event_sink_id)
                         find_key_cap(EventSink, params[:event_sink_id],cap("READ","EVENT_SINK")).
                           event_selectors.visible(cap("READ"),@current_user.id)
                       else
                         visible(model,cap("READ"))
                       end
    respond_to do |format|
      format.html { } 
      format.json { render api_index EventSelector, @event_selectors }
    end
  end

  def show
    @event_selector = find_key_cap(model, params[:id],cap("READ"))
    respond_to do |format|
      format.html {  }
      format.json { render api_show @event_selector }
    end
  end

  def update
    EventSelector.transaction do
      @event_selector= find_key_cap(model,params[:id],cap("UPDATE")).lock!
      if request.patch?
        patch(@event_selector,%w{selector tenant_id})
      else
        @event_selector.update_attributes!(params.permit(:selector, :tenant_id))
      end
    end
    render api_show @event_selector
  end

  def create
    params.require(:event_sink_id)
    params.require(:selector)
    params[:tenant_id] ||= @current_user.current_tenant_id
    EventSelector.transaction do
      validate_create(params[:tenant_id])
      event_sink = find_key_cap(EventSink,params[:event_sink_id],cap("UPDATE","EVENT_SINK"))
      @event_selector = EventSelector.create!(event_sink_id: event_sink.id,
					      tenant_id: params[:tenant_id],
                                              selector: params[:selector])
    end
    render api_show @event_selector
  end

  def destroy
    model.transaction do
      @es = find_key_cap(model, params[:id],cap("DESTROY"))
      @es.destroy
    end
    render api_delete @es
  end

end
