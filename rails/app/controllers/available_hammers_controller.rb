# Copyright 2014, Dell
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
class AvailableHammersController < ApplicationController
  self.model = AvailableHammer
  self.cap_base = "AVAILABLE_HAMMER"

  # API GET /api/v2/available_hammers
  def index
    @hammers = AvailableHammer.order('priority')
    respond_to do |format|
      format.html { } # show.html.erb
      format.json { render api_index AvailableHammer, @hammers }
    end
  end

  def show
    @hammer = AvailableHammer.find_key params[:id]
    respond_to do |format|
      format.html {  }
      format.json { render api_show @hammer }
    end
  end

end
