# Copyright 2014, Victor Lowther
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
class HammersController < ApplicationController
  self.model = Hammer
  self.cap_base = "NODE"
 
  # API GET /api/v2/hammers
  def index
    @hammers = if params.has_key?(:node_id)
                 find_key_cap(Node, params[:node_id],cap("READ")).hammers
               else
                 visible(model,cap("READ"))
               end
    respond_to do |format|
      format.html { } 
      format.json { render api_index Hammer, @hammers }
    end
  end

  def show
    @hammer = find_key_cap(model, params[:id],cap("READ"))
    respond_to do |format|
      format.html {  }
      format.json { render api_show @hammer }
    end
  end

  def update
    Hammer.transaction do
      @nm = find_key_cap(model, params[:id], cap("UPDATE")).lock!
      simple_update(@nm,%w{priority endpoint username authenticator})
    end
    render api_show @nm
  end

  def create
    params.require(:node_id)
    params.require(:available_hammer_id)
    params.require(:username)
    Hammer.transaction do
      @node = find_key_cap(Node,
                           params[:node_id],
                           cap("UPDATE"))
      @available_hammer = find_key_cap(AvailableHammer,
                                       params[:available_hammer_id],
                                       cap("READ","AVAILABLE_HAMMER"))
      @hammer = Hammer.create!(node: @node,
                               available_hammer: @available_hammer,
                               username: params[:username])
      @hammer.update_attributes!(params.permit(:username,
                                               :endpoint,
                                               :priority,
                                               :authenticator))
    end
    render api_show @hammer
  end

  def perform
    @nm = Hammer.find_key(params[:hammer_id])
    res = {}
    params.require(:method)
    action = params[:method].to_sym
    args = params[:args] || []
    res['node'] = @nm.node.name
    res['name'] = @nm.name
    res['id'] = @nm.id
    res['method'] = action
    res['args'] = args
    res['result_type'] = 'action'
    begin
      known_actions = (@nm.actions.values || []).flatten
      raise "#{@nm.class.name} has no #{action} method." unless known_actions.member?(action)
      res['results'] = if @nm.respond_to?(:__perform)
                         @nm.__perform(action,*args)
                       else
                         @nm.send(action,*args).as_json
                       end
    rescue => e
      res['result_type'] = 'exception'
      res['results'] = e.inspect
    end
    render :json => res, :content_type => cb_content_type(@nm,'action')
  end

  def destroy
    model.transaction do
      @hammer = find_key_cap(model, params[:hammer_id] || params[:id], cap("UPDATE"))
      @hammer.delete
    end
    render api_delete @hammer
  end
end
