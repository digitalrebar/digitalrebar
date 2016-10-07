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

require 'digest/md5'
require 'open4'

class Node < ActiveRecord::Base

  after_create      :load_uuid

  before_validation :default_population
  before_destroy    :before_destroy_handler
  after_update      :deployment_change_handler
  after_commit      :after_create_hooks, on: :create
  after_commit      :after_commit_handler, on: :update
  after_commit      :on_destroy_hooks, on: :destroy

  # Make sure we have names that are legal
  # requires at least three domain elements "foo.bar.com", cause the admin node shouldn't
  # be a top level domain ;p
  FQDN_RE = /\A([a-zA-Z0-9_\-]{1,63}\.){2,}(?:[a-zA-Z]{2,})\z/
  # for to_api_hash
  API_ATTRIBUTES = ["id", "name", "description", "order", "admin", "available", "alive",
                    "allocated", "created_at", "updated_at"]
  #
  # Validate the name should unique (no matter the case)
  # and that it starts with a valid FQDN
  #
  validates_format_of     :name, :with=>FQDN_RE, :message => I18n.t("db.fqdn", :default=>"Name must be a fully qualified domain name.")
  validates_length_of     :name, :maximum => 255

  has_and_belongs_to_many :groups, :join_table => "node_groups", :foreign_key => "node_id"

  has_many    :node_roles,         :dependent => :destroy
  has_many    :runs,               :dependent => :destroy
  has_many    :roles,              :through => :node_roles
  has_many    :deployments,        :through => :node_roles
  has_many    :network_allocations,:dependent => :destroy
  has_many    :hammers,            :dependent => :destroy
  belongs_to  :deployment
  belongs_to  :provider
  belongs_to  :target_role,        :class_name => "Role", :foreign_key => "target_role_id"  # used to troubleshoot problem nodes (see API doc)

  alias_attribute :ips,            :network_allocations

  scope    :admin,              -> { where(:admin => true) }
  scope    :alive,              -> { where(:alive => true) }
  scope    :available,          -> { where(:available => true) }
  scope    :regular,            -> { where(:admin => false, :system=>false) }
  scope    :non_system,         -> { where(:system=>false) }
  scope    :system,             -> { where(:system => true) }
  scope    :category,           -> (cat) { joins(:groups).where(["groups.category = ?", cat]) }
  scope    :where_jsonb,        -> (mv) do
    where(mv.map{|k,v| "((nodes.discovery #> '#{k}' = #{Node.sanitize(v.to_json)}) OR (nodes.hint #> '#{k}' = #{Node.sanitize(v.to_json)}))"}.join(" AND "))
  end

  def self.params_to_mv(params)
    mv = {}
    attrs = Attrib.where(role_id: nil, name: params.keys.map{|k|k.to_s})
    attrs.each do |a|
      mv[a.where_jsonb] = params[a.name.to_sym]
    end
    return mv
  end

  def as_json(args = nil)
    args ||= {}
    args[:except] = [ :discovery, :hint, :order, :notes ]
    o = super(args)
    o['node-control-address'] = get_attrib('node-control-address')
    o['state'] = state
    o
  end

  # Get all the attributes applicable to a node.
  # This includes:
  # * All attributes that are defined for our node roles, by virtue of
  #   being defined as part of the role that the node role is bound to, and
  # * All attributes that are not defined as part of a node.
  def attribs
    Attrib.where(["attribs.role_id IS NULL OR attribs.role_id in (select role_id from node_roles where node_id = ?)",self.id])
  end

  def <=>(other)
    self.name <=> other.name
  end

  # look at Node state by scanning all node roles.
  def state
    Node.transaction do
      # first look for single items that change the whole node
      node_roles.each do |nr|
        return nr.state if [NodeRole::TRANSITION, NodeRole::ERROR, NodeRole::PROPOSED].include? nr.state
      end
      # then scan for secondary items (ordering could hide the earlier items)
      node_roles.each do |nr|
        return NodeRole::TODO if [NodeRole::BLOCKED, NodeRole::TODO].include? nr.state
      end
    end
    # fall through (all NRs must be active)
    return NodeRole::ACTIVE
  end

  # returns a hash with all the node error status information
  def status
    s = []
    Node.transaction do
      node_roles.each { |nr| s[nr.id] = nr.status if nr.error?  }
    end
  end

  def shortname
    self.name.split('.').first
  end

  def login
    "root@#{shortname}"
  end

  def v6_hostpart
    d = Digest::MD5.hexdigest(name)
    "#{d[16..19]}:#{d[20..23]}:#{d[24..27]}:#{d[28..32]}"
  end

  def auto_v6_address(net)
    return nil if net.v6prefix.nil?
    IP.coerce("#{net.v6prefix}:#{v6_hostpart}/64")
  end

  def addresses(ip_type_filter = :all, networks = ["admin"], return_filter = :all)
    res = []
    networks.each do |net_cat|
      nets = Network.in_category(net_cat)
      nets.each do |net|
        res2 = network_allocations.where(network_id: net.id).select do |a|
          answer = true
          answer = false if ip_type_filter == :v4_only and !a.address.v4?
          answer = false if ip_type_filter == :v6_only and !a.address.v6?
          answer
        end.map do |a|
          a.address
        end

        if res2
          res2 = res2.sort
          res << res2
        end
      end
    end
    if return_filter == :all or return_filter == :public
      control_address = Attrib.get('node-control-address',self)
      res << IP.coerce(control_address) if control_address
    end
    if return_filter == :all or return_filter == :private
      private_control_address = Attrib.get('node-private-control-address',self)
      res << IP.coerce(private_control_address) if private_control_address
    end
    res.flatten
  end

  def address(ip_type_filter = :all, networks = ["admin"], return_filter = :all)
    res = addresses(ip_type_filter,networks,return_filter).detect{|a|a.reachable?}
    Rails.logger.warn("Node #{name} did not have any reachable addresses in networks #{networks.inspect}") unless res
    res
  end

  def url_address
    res = address
    (res.v6? ? "[#{res.addr}]" : res.addr).to_s
  end

  #
  # Helper function to test admin without calling admin. Style-thing.
  #
  def is_admin?
    admin
  end

  def is_system?
    system
  end

  def virtual?
    virtual = [ "KVM", "VMware Virtual Platform", "VMWare Virtual Platform", "VirtualBox" ]
    virtual.include? get_attrib('hardware')
  end

  # retrieves the Attrib from Attrib
  def get_attrib(attrib)
    Attrib.get(attrib, self) rescue nil
  end

  def merge_quirks(new_quirks)
    self.quirks = (self.quirks + new_quirks).sort.uniq
    save!
  end

  def active_node_roles
    NodeRole.on_node(self).in_state(NodeRole::ACTIVE).committed.order("cohort ASC")
  end

  def all_active_data
    dres = {}
    res = {}
    Node.transaction(read_only: true) do
      active_node_roles.each do |nr|
        dres.deep_merge!(nr.deployment_data)
        res.deep_merge!(nr.all_parent_data)
      end
    end
    dres.deep_merge(res)
  end

  def actions
    @nodemgr_actions = Hammer.gather(self) unless @nodemgr_actions
    @nodemgr_actions
  end

  def halt_if_bored(nr)
    return unless power[:on]
    return unless nr.children.empty? || nr.children.all?{|nr|nr.proposed?}
    return if get_attrib("stay_on")
    Rails.logger.info("Node #{self.name} is bored, powering off.")
    self.bootenv == "local" ? power.halt : power.off
  end

  def power
    actions[:power] || {}
  end

  def transfer
    actions[:xfer] || {}
  end

  def run(cmd, nr=nil)
    raise("No run actions for #{name}") unless actions[:run]
    actions[:run].run(cmd, nr)
  end

  def ssh(cmd, nr=nil)
    Rails.logger.warn("Node.ssh outdated, please update #{caller[0]} to use Node.run instead!")
    run(cmd, nr)
  end

  def scp_from(remote_src, local_dest, opts="")
    Rails.logger.warn("Node.scp_from outdated, please update #{caller[0]} to use Node.transfer.copy_from instead!")
    transfer.copy_from(remote_src,local_dest,opts)
  end

  def scp_to(local_src, remote_dest, opts="")
    Rails.logger.warn("Node.scp_to outdated, please update #{caller[0]} to use Node.transfer.copy_to instead!")
    transfer.copy_to(local_src,remote_dest,opts)
  end

  def self.name_hash
    Digest::SHA1.hexdigest(Node.select(:name).order("name ASC").map{|n|n.name}.join).to_i(16)
  end


  def method_missing(m,*args,&block)
    method = m.to_s
    if method.starts_with? "attrib_"
      return get_attrib method[7..-1]
    else
      super
    end
  end

  def <=>(other)
    # use Array#<=> to compare the attributes
    [self.order, self.name] <=> [other.order, other.name]
  end

  def group
    groups.first
  end

  def group=(group)
    Group.transaction do
      db_group = group.is_a?(Group) ? group : Group.find_or_create_by_name({'name' => group, 'description' => group, 'category' => 'ui', 'tenant_id' => tenant_id})
      if db_group
        category = db_group.category
        groups.each { |g| g.nodes.delete(self) if g.category.eql?(category) }
        groups << db_group unless db_group.nodes.include? self
      end
    end
  end

  def note_update(val)
    transaction do
      self.notes = self.notes.deep_merge(val)
      save!
    end
  end

  def hint_update(val)
    Node.transaction do
      self.hint = self.hint.deep_merge(val)
      save!
    end
  end

  def discovery_merge(val)
    Node.transaction do
      self.discovery = self.discovery.merge(val)
      save!
    end
  end

  def discovery_update(val)
    Node.transaction do
      self.discovery = self.discovery.deep_merge(val)
      save!
    end
  end

  def debug
    Node.transaction do
      reload
      update!(alive: true,
              bootenv: "sledgehammer",
              target: Role.find_by!(:name => "rebar-managed-node"))
    end
    provider.reboot
  end

  def undebug
    Node.transaction do
      reload
      update!(alive: false,
              bootenv: "local",
              target: nil)
    end
    provider.reboot
  end

  def is_docker_node?
    #
    # TODO: This code will need to be refactored once node types are added.
    #
    # the is_docker_node should be replaced with something related to type.
    # Maybe something like quirks.  Matching role "quirks" with node "quirks"
    #
    is_docker_node = false
    node_roles.each do |nr|
      if nr.role.name == "rebar-joined-node"
        is_docker_node = true
        break
      end
    end
    is_docker_node
  end

  def propose!
    Node.transaction do
      node_roles.order("cohort ASC").each do |nr|
        nr.propose!
      end
    end
  end

  def commit!(ignore_power=false)
    Role.all_cohorts.each do |r|
      if (!system && !admin && !is_docker_node? && r.discovery)
        r.add_to_node(self)
      end
    end

    Node.transaction do
      reload
      update!(available: true)
      node_roles.order("cohort ASC").each do |nr|
        nr.commit!(ignore_power)
      end
    end
  end

  def scrub!
    Node.transaction do
      to_scrub = node_roles.where(["deployment_id != :depl AND deployment_id NOT IN (select parent_id from all_deployment_parents where child_id = :depl)", {depl:  self.deployment_id}]).
                 order('cohort DESC')
      to_scrub.each do |ts|
        ts.destroy
      end
    end
  end

  def redeploy!
    Rails.logger.debug("Starting Redeploy for #{name}")
    Node.transaction do
      Rails.logger.debug("redeploy: reloading #{name}")
      reload
      Rails.logger.debug("redeploy: update bootenv for #{name}")
      update!(bootenv: "sledgehammer")
    end
    Rails.logger.debug("redeploy: restart node for #{name}")
    if actions[:power][:cycle]
      Rails.logger.debug("redeploy: using cycle for #{name}")
      actions[:power].cycle
    elsif actions[:power][:reset]
      Rails.logger.debug("redeploy: using reset for #{name}")
      actions[:power].reset
    else
      Rails.logger.debug("redeploy: using reboot for #{name}")
      actions[:power].reboot
    end
    Node.transaction do
      Rails.logger.debug("redeploy: reloading2 #{name}")
      reload
      Rails.logger.debug("redeploy: update all node roles for #{name}")
      node_roles.update_all(run_count: 0, state: NodeRole::PROPOSED)
    end
    Rails.logger.debug("redeploy: commit node #{name}")
    val = commit!(true)
    Rails.logger.debug("redeploy: done for #{name}")
    val
  end

  def target
    return target_role
  end
  # Set the annealer target for this node, which will restrict the annealer
  # to considering the noderole for this role bound to this node and its parents
  # for converging.  If nil is passed, then all the noderoles are marked as available.
  def target=(r)
    Node.transaction do
      reload
      if r.nil?
        old_alive = self.alive
        self.save!
        node_roles.each do |nr|
          nr.available = true
        end
        self.target_role_id = nil
        self.alive = old_alive
        self.save!
        return self
      elsif r.kind_of?(Role) &&
          roles.member?(r) &&
          r.barclamp.name == "rebar" &&
          r.jig.name == "noop"
        old_alive = self.alive
        self.alive = false
        self.save!
        self.target_role = r
        node_roles.each do |nr|
          nr.available = false
        end
        target_nr = self.node_roles.where(:role_id => r.id).first
        target_nr.all_parents.each do |nr|
          next unless nr.node_id == self.id
          nr.available = true
        end
        target_nr.available = true
        self.save!
        return self
      else
        raise("Cannot set target role #{r.name} for #{self.name}")
      end
    end
  end

  def alive?
    return false if alive == false
    return true if self.is_system?
    return true unless Rails.env == "production"
    a = address
    return true if a && self.run("echo alive")[2].success?
    Node.transaction do
      self[:alive] = false
      save!
    end
    false
  end

  private

  def deployment_change_handler
    return unless self.deployment_id_changed?
    # If we change deployments from system to something else, then
    # make proposed noderoles follow into the new deployment if they have no
    # children that are not also proposed.
    Node.transaction do
      old_deployment = Deployment.find(self.changes["deployment_id"][0])
      new_deployment = Deployment.find(self.changes["deployment_id"][1])
      Rails.logger.info("Node: #{self.name} changed deployment_id from #{old_deployment.id} to #{new_deployment.id}")
      moving_noderoles = []
      node_roles.where(deployment_id: old_deployment.id, run_count: 0, state: NodeRole::PROPOSED).order("cohort ASC").each do |nr|
        Rails.logger.info("Node: testing to see if #{nr.name} should move")
        blocking_children = nr.all_children.where.not(["node_roles.run_count = 0 AND node_roles.deployment_id = ? AND node_roles.state = ?",
                                                       old_deployment.id,
                                                       NodeRole::PROPOSED])
        unless blocking_children.empty?
          Rails.logger.info("Node: #{nr.name} cannot move even though it is a candidate.")
          Rails.logger.info("Move is blocked by:")
          blocking_children.each do |c|
            Rails.logger.info("  #{c.name}: #{c.deployment.name}, #{c.run_count}, #{c.state_name}")
          end
          next
        end
        Rails.logger.info("Node: #{nr.name} should change deployment")
        moving_noderoles << nr
      end
      moving_noderoles.each do |nr|
        nr.role.add_to_deployment(new_deployment)
        nr.deployment_id = new_deployment.id
        nr.save!
      end
    end
  end

  def after_commit_handler
    Rails.logger.debug("Node: after_commit hook called")
    Rails.logger.info("Node: calling all role on_node_change hooks for #{name}")
    # the line belowrequires a rebar deployment to which the status attribute is tied
    Group.transaction do
      if groups.count == 0
        groups << Group.find_or_create_by(name: 'not_set',
                                          tenant_id: tenant_id,
                                          description: I18n.t('not_set', :default=>'Not Set'))
      end
    end
    # We only call on_node_change when the node is available to prevent Rebar
    # from noticing changes it should not notice yet
    if available?
      Event.fire(self, event: 'on_node_change')
    end
    Event.fire(self, event: 'on_node_move') if previous_changes[:deployment_id]
    if (previous_changes[:alive] || previous_changes[:available])
      if alive && available && node_roles.runnable.count > 0
        Rails.logger.info("Node: #{name} is alive and available, kicking the annealer.")
        Run.run!
      elsif previous_changes[:alive] && !alive?
        Rails.logger.info("Node: #{name} is not alive, deactivating noderoles on this node.")
        NodeRole.transaction do
          node_roles.order("cohort ASC").each do |nr|
            nr.deactivate
          end
          runs.delete_all
        end
      end
    end
  end

  # make sure some safe values are set for the node
  def default_population
    self.name = self.name.downcase
    self.deployment ||= Deployment.system
  end

  # Call the on_node_delete hooks.
  def on_destroy_hooks
    # do the low cohorts last
    begin
      Event.fire(self, event: 'on_node_delete')
    rescue StandardError => e
      Rails.logger.error "Node #{name}: on_node_delete failed with #{e.message}"
      Rails.logger.error "StackTrace: #{e.backtrace.join("\n")}"
    end
  end

  def after_create_hooks
    return if @after_create
    @after_create = true
    self.provider.create_node(self)
    # Handle binding the default hammers for this node.
    case
    when self.is_system?
      # System nodes do not actually exist, so they do not get default hammers.
      true
    when self.os_family == "linux"
      # Linux systems start off with the ssh hammer.
      Hammer.bind(manager_name: "ssh", username: "root", node: self)
    else
      raise "No idea which hammer to bind by default"
    end
    # Call all role on_node_create hooks with self.
    # These should happen synchronously.
    # do the low cohorts first
    Event.fire(self, event: 'on_node_create')
    Role.all_cohorts.where(bootstrap: true).each do |r|
      Rails.logger.info("Node: Adding #{r.name} to #{self.name} (bootstrap)")
      r.add_to_node(self)
    end if admin
  end

  def before_destroy_handler
    Node.transaction do
      return false if self.admin && Node.admin.count == 1
      # Delete all the noderoles bound in this deployment
      node_roles.order("cohort DESC").each do |nr|
        return false unless nr.destroy
      end
      self.provider.delete_node(self)
    end
  end

  def load_uuid
    self.reload
  end

end
