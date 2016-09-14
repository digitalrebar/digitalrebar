# Copyright 2013, Dell
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
class RunsController < ApplicationController
  self.model = Run
  self.cap_base = "NODE"

  def index
    @runs = visible(model,cap("READ"))
    respond_to do |format|
      format.json { render api_index model, @runs }
    end
  end

  # returns all the items for a node in the annealer queue
  def show
    runs = find_key_cap(Node, params[:id], cap("READ")).runs
    respond_to do |format|
      format.json { render api_index model, runs }
    end
  end

end
