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
  respond_to :json

  def index
    @list = if params.has_key? :network_id or params.has_key? :network
              network =  Network.find_key params[:network_id] || params[:network]
              network.network_allocations
            else
              NetworkAllocation.all
            end
    respond_to do |format|
      format.html { }
      format.json { render api_index NetworkAllocation, @list }
    end
  end

  def show
    @allocation = network.network_allocations.find_key(params[:id]) rescue nil
    respond_to do |format|
      format.html {
                    @list = [@allocation]
                    render :action=>:index
                  }
      format.json { render api_show @allocation }
    end
  end

end
