# Copyright 2015, Greg Althaus
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
class NetworkRoutersController < ::ApplicationController
  respond_to :json

  def index
    @list = if params.has_key? :network_id or params.has_key? :network
              network =  Network.find_key params[:network_id] || params[:network]
              [ network.router ]
            else
              NetworkRouter.all
            end
    respond_to do |format|
      format.json { render api_index NetworkRouter, @list }
    end
  end

  def show
    network = Network.find_key params[:network_id]
    data = [ network.router.address.to_s ]
    respond_to do |format|
      format.json { render :json => data }
    end
  end

end

