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
  
  def nodes
    g = Group.find_key params[:id]
    render api_index :nodes, g.nodes
  end

  def index
    @list = if params.has_key? :node_id
      n = Node.find_key params[:node_id]
      n.groups
    else
      Group.all
    end
    respond_to do |format|
      format.html { }
      format.json { render api_index Group, @list }
    end
  end

  def show
    if params.has_key? :node_id
      # this should use the _path
      redirect_to groups_path(:id=>params[:id])
    else
      @group = Group.find_key params[:id]
      render api_show @group
    end
  end
  
  def create
    if params.has_key? :node_id
      render api_not_supported 'put', 'nodes/:id/groups/:id'
    else
      params.require(:name)
      @group = Group.create! params.permit(:name, :description, :category)
      render api_show @group
    end
  end
  
  def update
    if params.has_key? :node_id
      # TODO this needs to be restricted to the node only
      g = Group.find_key params[:id]
      n = Node.find_key params[:node_id]
      n.groups << g  if g and n
      render :text=>I18n.t('api.added', :item=>g.name, :collection=>'node.groups')
    else
      @group = Group.find_key(params[:id])
      @group.update_attributes!(params.permit(:name, :description, :category))
      render api_show @group
    end
  end

  def destroy
    if params.has_key? :node_id
      # TODO this needs to be restricted to the node only
      g = Group.find_key params[:id]
      n = Node.find_key params[:node_id]
      g.nodes.delete(n) if g.nodes.include? n
      render :text=>I18n.t('api.removed', :item=>'node', :collection=>'group')
    else
      @group = Group.find_key(params[:id])
      @group.destroy
      render api_delete @group
    end
  end

  
end
