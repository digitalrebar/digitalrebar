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


  def sample
    render api_sample(EventSelector)
  end

  def match
    attrs = EventSelector.attribute_names.map{|a|a.to_sym}
    objs = []
    ok_params = params.permit(attrs)
    objs = validate_match(ok_params, :tenant_id, "EVENT_SELECTOR", EventSelector)
    respond_to do |format|
      format.html {}
      format.json { render api_index EventSelector, objs }
    end
  end

  def index
    @event_selectors = if params.key(:event_sink_id)
                         EventSink.find_key(params[:event_sink_id]).event_selectors.to_a
                       else
                         EventSelector.all.to_a
                       end
    t_ids = build_tenant_list("EVENT_SELECTOR_READ")
    @event_selectors.delete_if { |x| !t_ids.include? x.tenant_id }
    respond_to do |format|
      format.html { } 
      format.json { render api_index EventSelector, @event_selectors }
    end
  end

  def show
    @event_selector = EventSelector.find_key(params[:id])
    validate_read(@event_selector.tenant_id, "EVENT_SELECTOR", EventSelector, params[:id])
    respond_to do |format|
      format.html {  }
      format.json { render api_show @event_selector }
    end
  end

  def update
    EventSelector.transaction do
      @event_selector= event.find_key(params[:id]).lock!
      validate_update(@event_selector.tenant_id, "EVENT_SELECTOR", EventSelector, params[:id])
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
    unless params[:tenant_id]
      params[:tenant_id] = @current_user.tenant_id
    end
    validate_create(params[:tenant_id], "EVENT_SELECTOR", EventSelector)
    event_sink = EventSink.find_key(params[:event_sink_id])
    EventSelector.transaction do
      @event_selector = EventSelector.create!(event_sink_id: event_sink.id,
					      tenant_id: params[:tenant_id],
                                              selector: params[:selector])
    end
    render api_show @event_selector
  end

  def destroy
    @es = EventSelector.find_key(params[:id])
    validate_destroy(@event_selector.tenant_id, "EVENT_SELECTOR", EventSelector, params[:id])
    @es.destroy
    render api_delete @es
  end

end
