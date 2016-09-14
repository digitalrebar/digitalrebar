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
class DashboardController < ApplicationController

  # GREG: FIGURE OUT THE TENANT CRAP HERE

  def layercake
    # we may want to move this into the database at some point
    taxmap = JSON::load File.open(File.join("config", "layercake.json"), 'r')
    @layers = {}
    @status = {}
    taxmap["layers"].each { |k| @layers[k]=[] }
    NodeRole.all.joins(:node,:role).select("node_roles.*,
                                      roles.name as role_name,
                                      nodes.name as node_name,
                                      nodes.alive as node_alive,
                                      nodes.available as node_available").each do |nr|
      layer = taxmap[nr.role_name] || 'apps'
      @layers[layer] << nr 
      if nr.state == NodeRole::ERROR
        @status[layer] = 'alert'
      elsif (nr.state == NodeRole::PROPOSED)
        @status[layer] = 'user' unless @status[layer] == 'alert'
      elsif nr.state != NodeRole::ACTIVE
        @status[layer] = 'system' unless @status[layer] == 'alert'
      end
    end

    respond_to do |format|
      format.html { }
      format.json { render :json => @status.to_json } # also respond to json for layercake ajax updating
    end
  end    

  def group_change
    # TODO: not used?
    node = Node.find_by_name params[:id]
    if node.nil?
      raise "Node #{params[:id]} not found.  Cannot change group" 
    else
      group = params[:group]
      if params.key? 'automatic'
        node.group=""
      else
        node.group=group
      end
      node.save
      Rails.logger.info "node #{node.name} changed its group to be #{node.group.empty? ? 'automatic' : group}."
      render :inline => "SUCCESS: added #{node.name} to #{group}.", :cache => false 
    end
  end
  

  # Bulk Edit
  def list
    if request.put?
      nodes = {}
      params.each do |k, v|
        if k.starts_with? "node:"
          parts = k.split ':'
          node = parts[1]
          area = parts[2]
          nodes[node] ||= {} 
          nodes[node][area] = v
        end
      end
      succeeded = []
      failed = []
      nodes.each do |node_name, values|
        node = Node.find_key node_name
        begin
          node.update_attributes! values
          succeeded << node.name
        rescue StandardError=>e
          Rails.logger.info "user attempted dashboard.list put for node #{node.name} raised error #{e.message}"
          failed << node.name
        end
      end
      if failed.length>0
        flash[:notice] = I18n.t('failed', :scope=>'dashboard.list', :list=>failed.join(','))
      elsif succeeded.length>0
        flash[:notice] = I18n.t('updated', :scope=>'dashboard.list', :list=>succeeded.join(','))
      end
    end
    @nodes = if params.key? :deployment
      @deployment = Deployment.find_key params[:deployment]
      @deployment.nodes.where(:system=>false)
    else
      Node.all.where(:system=>false)
    end
  end

  # group nodes into categories
  def families

    # the attribs list is passed as the ID w/ pipe delimiters
    @families = if params[:id]
      params[:id].split "|"
    else
      ['cpu_count', 'memory',  'number_of_drives']
    end
    @nodes = {}
    # this works by building a single key with all the requested attributes in order
    # that makes it easy to sort and collect like attributes
    Node.all.each do |n|
      next if n.system
      key = @families.map { |f| (n.get_attrib(f) || t("unknown")).to_s }
      @nodes[key] ||= {}
      @nodes[key][n.id] = n.name
    end
  end

end
