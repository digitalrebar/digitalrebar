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
class GroupsController < ApplicationController
  
  def index
    @list = if params.has_key? :node_id
      n = Node.find_key params[:node_id]
      if validate_capability(n.tenant_id, "NODE_READ")
        n.groups
      else
        []
      end
    else
      Group.all.to_a
    end
    t_ids = build_tenant_list("GROUP_READ")
    @list.delete_if { |x| !t_ids.include? x.tenant_id }
    respond_to do |format|
      format.html { }
      format.json { render api_index Group, @list }
    end
  end

  def show
    @group = Group.find_key params[:id]
    validate_read(@group.tenant_id, "GROUP", Group, params[:id])
    respond_to do |format|
      format.html { }
      format.json { render api_show @group }
    end
  end
  
  def create
    params.require(:name)
    params[:category] = params[:category].first if params[:category].kind_of?(Array)
    unless params[:tenant_id]
      params[:tenant_id] = @current_user.tenant_id
    end
    validate_create(params[:tenant_id], "GROUP", Group)
    @group = Group.create! params.permit(:name, :description, :category, :tenant_id)
    respond_to do |format|
      format.html { redirect_to group_path(@group.id)}
      format.json { render api_show @group }
    end
  end
  
  def update
    params[:category] = params[:category].first if params[:category].kind_of?(Array)
    @group = Group.find_key(params[:id])
    validate_update(@group.tenant_id, "GROUP", Group, params[:id])
    @group.update_attributes!(params.permit(:name, :description, :category, :tenant_id))
    render api_show @group
  end

  def destroy
    @group = Group.find_key(params[:id])
    validate_destroy(@group.tenant_id, "GROUP", Group, params[:id])
    @group.destroy
    render api_delete @group
  end
  
end
