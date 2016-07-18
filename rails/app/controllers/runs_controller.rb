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

  # returns all the items in the annealer queue
  def match
    attrs = Run.attribute_names.map{|a|a.to_sym}
    objs = []
    ok_params = params.permit(attrs)
    objs = Run.where(ok_params) if !ok_params.empty?
    respond_to do |format|
      format.html {}
      format.json { render api_index Run, objs }
    end
  end

  def index
    respond_to do |format|
      format.json { render api_index :run, Run.all }
    end
  end

  # returns all the items for a node in the annealer queue
  def show
    node = Node.find_key params[:id]
    runs = (node.nil? ? [] : node.runs)
    respond_to do |format|
      format.json { render api_index :run, runs }
    end
  end

end
