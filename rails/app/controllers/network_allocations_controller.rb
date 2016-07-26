# Copyright 2015, RackN
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
class NetworkAllocationsController < ::ApplicationController
  self.model = NetworkAllocation
  self.cap_base = "NETWORK"

  def create
    ret = nil
    model.transaction do
      params.require(:node_id)
      node = find_key_cap(Node, params[:node_id], cap("UPDATE","NODE"))
      suggestion = params[:address]
      suggestion = nil if suggestion && suggestion == ""
      ret = if params[:network_range_id]
              find_key_cap(NetworkRange, params[:network_range_id], cap("ALLOCATE")).
                allocate(node,suggestion)
            elsif params[:network_id]
              find_key_cap(Network,params[:network_id], cap("ALLOCATE")).
                auto_allocate(node)
            else
              raise "Need a network or range"
            end
    end
    render :json => ret
  end

  def destroy
    params.require(:id)
    model.transaction do
      @allocation = find_key_cap(model,params[:id],cap("UPDATE"))
      # Called for side effect.
      find_key_cap(Node,@allocation.node.id,cap("UPDATE","NODE"))
      @allocation.destroy
    end
    render api_delete @allocation
  end

  def index
    model.transaction do
      @list = if params.has_key? :network_id or params.has_key? :network
                find_key_cap(Network,params[:network_id] || params[:network],cap("READ")).
                  network_allocations
              elsif params.has_key?(:network_range_id)
                find_key_cap(NetworkRange, params[:network_range_id],cap("READ")).
                  network_allocations
              elsif params.has_key?(:node_id) || params.has_key?(:node)
                find_key_cap(Node, params[:node_id] || params[:node], cap("READ","NODE")).
                  network_allocations.visible(cap("READ"),@current_user.id)
              else
                visble(model,cap("READ"))
              end
    end
    respond_to do |format|
      format.html { }
      format.json { render api_index model, @list }
    end
  end

  def show
    @allocation = find_key_cap(model, params[:id], cap("READ"))
    respond_to do |format|
      format.html {
        # Why?
        @list = [@allocation]
        render :action=>:index
      }
      format.json { render api_show @allocation }
    end
  end

end
