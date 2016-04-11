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


  def sample
    render api_sample(EventSink)
  end

  def match
    attrs = EventSink.attribute_names.map{|a|a.to_sym}
    objs = []
    ok_params = params.permit(attrs)
    objs = EventSink.where(ok_params) if !ok_params.empty?
    respond_to do |format|
      format.html {}
      format.json { render api_index Event, objs }
    end
  end

    # API GET /api/v2/hammers
  def index
    @event_sinks = EventSink.all
    respond_to do |format|
      format.html { } 
      format.json { render api_index EventSink, @event_sinks }
    end
  end

  def show
    @event_sink = EventSink.find_key(params[:id])
    respond_to do |format|
      format.html {  }
      format.json { render api_show @event_sink }
    end
  end

  def update
    EventSink.transaction do
      @event_sink= event.find_key(params[:id]).lock!
      if request.patch?
        patch(@event_sink,%w{endpoint username authenticator})
      else
        @event_sink.update_attributes!(params.permit(:endpoint,
                                                :username,
                                                :authenticator,
                                                :notes))
      end
    end
    render api_show @event_sink
  end

  def create
    params.require(:endpoint)
    EventSink.transaction do
      @event_sink = EventSink.create!(params.permit(:endpoint,
                                               :username,
                                               :authenticator,
                                               :notes))
    end
    render api_show @event_sink
  end

  def destroy
    render api_delete EventSink
  end


end
