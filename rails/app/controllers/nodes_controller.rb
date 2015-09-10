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
class NodesController < ApplicationController

  # API GET /rebar/v2/nodes
  # UI GET /dashboard

  def sample
    render api_sample(Node)
  end
  
  def match
    attrs = Node.attribute_names.map{|a|a.to_sym}
    objs = []
    ok_params = params.permit(attrs)
    objs = Node.where(ok_params) if !ok_params.empty?
    respond_to do |format|
      format.html {}
      format.json { render api_index Node, objs }
    end
  end

  def index
    @list = case
            when params.has_key?(:group_id)
              Group.find_key(params[:group_id]).nodes
            when params.has_key?(:deployment_id)
              Deployment.find_key(params[:deployment_id]).nodes
            when params.has_key?(:role_id)
              Role.find_key(params[:role_id]).nodes
            when params.has_key?(:deployment_role_id)
              DeploymentRole.find_key(params[:deployment_role_id]).nodes
            else
              Node.all
            end
    respond_to do |format|
      format.html { @list.delete_if { |n| n.system }}
      format.json { render api_index Node, @list }
    end
  end

  # API /api/status/nodes(/:id)
  def status
    nodes = if params[:id]
      node = Node.find_key params[:id]
      nodes = Node.where :id=>node.id
    else
      nodes = Node.all
    end
    status = {}
    nodes.each do |n|
      state = n.state
      str = [
        t(n.alive ? "common.state.alive" : "common.state.dead"),
        t(n.available ? "common.state.available" : "common.state.reserved"),
        NodeRole.state_name(state)
      ].join("\n")
      status[n.id] = {
        :name => n.name,
        :state => state,
        :status => NodeRole::STATES[state],
        :strStatus => str
      }
    end


     render api_array status.to_json
    #render :text => status.to_json
  end

  def show
    @node = Node.find_key params[:id]
    respond_to do |format|
      format.html {  } # show.html.erb
      format.json { render api_show @node }
    end
  end

  # Get the addresses allocated to a node on a network.
  def addresses
    nodename = params[:node_id]
    @node = nodename == "admin" ?  Node.admin.where(:available => true).first : Node.find_key(nodename)
    if params[:network]
      @net = Network.find_key(params[:network])
      res = {
        "node" => @node.name,
        "network" => @net.name,
        "category" => @net.category,
        "addresses" => @net.node_allocations(@node).map{|a|a.to_s}
      }
      render :json => res, :content_type=>cb_content_type(:addresses, "object")
    else
      res = []

      if params[:category]
        nets = Network.in_category(params[:category])
      else
        nets = Network.all
      end

      nets.each do |n|
        ips = n.node_allocations(@node)
        next if ips.empty?

        res << {
            "node" => @node.name,
            "network" => n.name,
            "category" => n.category,
            "addresses" => ips.map{|a|a.to_s}
        }
      end

      render :json => res, :content_type=>cb_content_type(:addresses, "array")
    end
  end

  # RESTful DELETE of the node resource
  def destroy
    @node = Node.find_key(params[:id] || params[:name])
    if params[:group_id]
      g = Group.find_key params[:group_id]
      g.nodes.delete(@node) if g.nodes.include? @node
      render :text=>I18n.t('api.removed', :item=>'node', :collection=>'group')
    else
      @node.destroy
      respond_to do |format|
        format.html { redirect_to deployment_path(@node.deployment_id) }
        format.json { render api_delete @node }
      end
    end
  end

  def power
    @node = Node.find_key(params[:id] || params[:name] || params[:node_id])
    if request.put?
      params.require(:poweraction)
      @poweraction = params[:poweraction].to_sym
      if @node.power.include? @poweraction
        result = @node.power.send(@poweraction) rescue nil
        # special case for development
        if result.nil?
          render api_not_implemented(@poweraction, "see logs for internal error") unless Rails.env.development?
          result = "development faked"
        end
        render api_result({"id" => @node.id, "action" => @poweraction, "result" => result })
      else
        render api_not_implemented @node, @poweraction, @node.power.keys
      end
    elsif request.get?
      render api_array @node.power.keys
    end

  end

  def debug
    node_action :debug
  end

  def undebug
    node_action :undebug
  end

  def redeploy
    node_action :redeploy!
  end

  def propose
    node_action :propose!
  end

  def commit
    node_action :commit!
  end

  # RESTfule POST of the node resource
  def create
    if params[:group_id] and params[:node_id]
      g = Group.find_key params[:group_id]
      n = Node.find_key params[:node_id]
      n.groups << g if g and n
      render :text=>I18n.t('api.added', :item=>g.name, :collection=>'node.groups')
    else
      params[:deployment_id] = Deployment.find_key(params[:deployment]).id if params.has_key? :deployment
      params[:deployment_id] ||= Deployment.system
      params.require(:name)
      params.require(:deployment_id)
      default_net = nil
      Node.transaction do
        @node = Node.create!(params.permit(:name,
                                           :description,
                                           :admin,
                                           :deployment_id,
                                           :allocated,
                                           :alive,
                                           :system,
                                           :available,
                                           :bootenv))
        # Keep suport for mac and ip hints in short form around for legacy Sledgehammer purposes
        if params[:ip]
          default_net = Network.lookup_network(params[:ip]) ||
                        Network.find_by_name("unmanaged")
          Attrib.set("hint-#{default_net.name}-v4addr",@node,params[:ip]) if default_net
          Attrib.set("hint-admin-macs", @node, [params[:mac]]) if params[:mac]
        end
      end
      default_net.make_node_role(@node) if default_net
      render api_show @node
    end
  end

  def update
    Node.transaction do
      @node = Node.find_key(params[:id]).lock!
      if request.patch?
        patch(@node,%w{name description target_role_id deployment_id allocated available alive bootenv})
      else
        params[:node_deployment].each { |k,v| params[k] = v } if params.has_key? :node_deployment
        params[:deployment_id] = Deployment.find_key(params[:deployment]).id if params.has_key? :deployment
        if params.has_key?(:deployment_id) && params.has_key?(:old_deployment_id)
          old_deployment = Deployment.find_key(params[:old_deployment_id])
          raise "Node is not in old deployment #{params[:old_deployment_id]}" unless @node.deployment == old_deployment
        end
        @node.update_attributes!(params.permit(:name,
                                               :description,
                                               :target_role_id,
                                               :deployment_id,
                                               :allocated,
                                               :available,
                                               :alive,
                                               :bootenv))
      end
    end
    render api_show @node
  end

  #test_ methods support test functions that are not considered stable APIs
  def test_load_data

    @node = Node.find_key params[:id]
    # get the file
    file = File.join "test", "data", (params[:source] || "node_discovery") + ".json"
    raw = File.read file
    # cleanup
    mac = 6.times.map{ |i| rand(256).to_s(16) }.join(":")
    raw = raw.gsub /00:00:00:00:00:00/, mac
    # update the node
    json = JSON.load raw
    @node.discovery  = json
    @node.save!
    render api_show @node

  end

  private

  def node_action(meth)
    @node = Node.find_key(params[:id] || params[:name] || params[:node_id])
    @node.send(meth)
    render api_show @node
  end

end
