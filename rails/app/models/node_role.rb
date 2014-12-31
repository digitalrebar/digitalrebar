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

require 'json'

class NodeRole < ActiveRecord::Base

  after_commit :bind_cluster_children, on: [:create]
  after_commit :run_hooks, on: [:update, :create]
  after_commit :create_deployment_role, on: [:create]
  after_commit :poke_attr_dependent_noderoles, on: [:update]
  after_create :bind_needed_parents
  validate :role_is_bindable, on: :create
  validate :noderole_has_all_parents, on: :create
  validate :validate_conflicts, on: :create


  belongs_to      :node
  belongs_to      :role
  belongs_to      :deployment
  has_one         :barclamp,          :through => :role
  has_many        :attribs,           :through => :role
  has_many        :runs,              :dependent => :destroy

  # find other node-roles in this deployment using their role or node
  scope           :all_by_state,      ->(state) { where(['node_roles.state=?', state]) }
  # A node is runnable if:
  # It is in TODO.
  # It is in a committed deployment.
  scope           :committed,         -> { joins(:deployment).where('deployments.state' => Deployment::COMMITTED).readonly(false) }
  scope           :deactivatable,     -> { where(:state => [ACTIVE, TRANSITION, ERROR]) }
  scope           :in_state,          ->(state) { where('node_roles.state' => state) }
  scope           :not_in_state,      ->(state) { where(['node_roles.state != ?',state]) }
  scope           :available,         -> { where(:available => true) }
  scope           :runnable,          -> { available.committed.in_state(NodeRole::TODO).joins(:node).where('nodes.alive' => true, 'nodes.available' => true).joins(:role).joins('inner join jigs on jigs.name = roles.jig_name').readonly(false).where(['node_roles.node_id not in (select node_roles.node_id from node_roles where node_roles.state in (?, ?))',TRANSITION,ERROR]) }
  scope           :committed_by_node, ->(node) { where(['state<>? AND state<>? AND node_id=?', NodeRole::PROPOSED, NodeRole::ACTIVE, node.id])}
  scope           :in_deployment,       ->(deployment) { where(:deployment_id => deployment.id) }
  scope           :with_role,         ->(r) { where(:role_id => r.id) }
  scope           :on_node,           ->(n) { where(:node_id => n.id) }
  scope           :peers_by_state,    ->(ss,state) { in_deployment(ss).in_state(state) }
  scope           :peers_by_role,     ->(ss,role)  { in_deployment(ss).with_role(role) }
  scope           :peers_by_node,     ->(ss,node)  { in_deployment(ss).on_node(node) }
  scope           :peers_by_node_and_role,  ->(n,r) { on_node(n).with_role(r) }
  scope           :deployment_node_role,    ->(s,n,r) { where(['deployment_id=? AND node_id=? AND role_id=?', s.id, n.id, r.id]) }

  # make sure that new node-roles have require upstreams
  # validate        :deployable,        :if => :deployable?
  # node_role_pcms maps parent noderoles to child noderoles.
  has_and_belongs_to_many(:parents,
                          -> { reorder('cohort DESC') },
                          :class_name => "NodeRole",
                          :join_table => "node_role_pcms",
                          :foreign_key => "child_id",
                          :association_foreign_key => "parent_id")
  has_and_belongs_to_many(:children,
                          -> { reorder('cohort ASC') },
                          :class_name => "NodeRole",
                          :join_table => "node_role_pcms",
                          :foreign_key => "parent_id",
                          :association_foreign_key => "child_id")
  # node_role_all_pcms is a view that expands node_role_pcms
  # to include all of the parents and children of a noderole,
  # recursively.
  has_and_belongs_to_many(:all_parents,
                          -> { reorder('cohort DESC') },
                          :class_name => "NodeRole",
                          :join_table => "node_role_all_pcms",
                          :foreign_key => "child_id",
                          :association_foreign_key => "parent_id",
                          :delete_sql => "SELECT 1") # TODO: Figure out how to remove
  has_and_belongs_to_many(:all_children,
                          -> { reorder('cohort ASC') },
                          :class_name => "NodeRole",
                          :join_table => "node_role_all_pcms",
                          :foreign_key => "parent_id",
                          :association_foreign_key => "child_id",
                          :delete_sql => "SELECT 1") # TODO: Figure out how to remove

  # Parent and child links based on noderole -> noderole attribute dependency.
  has_many        :parent_attrib_links, class_name: "NodeRoleAttribLink", foreign_key: "child_id"
  has_many        :child_attrib_links,  class_name: "NodeRoleAttribLink", foreign_key: "parent_id"

  # State transitions:
  # All node roles start life in the PROPOSED state.
  # At deployment commit time, all node roles in PROPOSED that:
  #  1. Have no parent node role, or
  #  2. Have a parent in ACTIVE state
  # will be placed in TODO state, and all others will be placed in BLOCKED.
  #
  # The annealer will then find all node roles in the TODO state, set them
  # to TRANSITION, and hand them over to their appropriate jigs.
  #
  # If the operation for the node role succeeds, the jig will set the
  # node_role to ACTIVE, set all the node_role's BLOCKED children to TODO, and
  # wake up the annealer for another pass.
  #
  # If the operation for the node role fails, the jig will set the node_role to
  # ERROR, set all of its children (recursively) to BLOCKED, and no further
  # processing for that node role dependency tree will happen.

  ERROR      = -1
  ACTIVE     =  0
  TODO       =  1
  TRANSITION =  2
  BLOCKED    =  3
  PROPOSED   =  4
  STATES     = {
    ERROR => 'error',
    ACTIVE => 'active',
    TODO => 'todo',
    TRANSITION => 'transition',
    BLOCKED => 'blocked',
    PROPOSED => 'proposed'
  }

  class InvalidTransition < Exception
    def initialize(node_role,from,to,str=nil)
      @errstr = "#{node_role.name}: Invalid state transition from #{NodeRole.state_name(from)} to #{NodeRole.state_name(to)}"
      @errstr += ": #{str}" if str
    end
    def to_s
      @errstr
    end

    def to_str
      to_s
    end
  end

  class InvalidState < Exception
  end

  class MissingJig < Exception
    def initalize(nr)
      @errstr = "NodeRole #{nr.name}: Missing jig #{nr.role.jig_name}"
    end
    def to_s
      @errstr
    end
    def to_str
      to_s
    end
  end

  # lookup i18n version of state
  def state_name
    NodeRole.state_name(state)
  end

  def self.state_name(state)
    raise InvalidState.new("#{state || 'nil'} is not a valid NodeRole state!") unless state and STATES.include? state
    I18n.t(STATES[state], :scope=>'node_role.state')
  end

  def self.find_needed_parents(target_role,target_node,target_dep)
    NodeRole.transaction do
      res = []
      target_role.parents.each do |parent|
        tenative_parent = NodeRole.find_by(role_id: parent.id, node_id: target_node.id) ||
          NodeRole.find_by("node_id = ? AND role_id in
                                            (select id from roles where ? = ANY(provides))",
                           target_node.id,
                           parent.name)
        unless parent.implicit?
          cdep = target_dep
          until tenative_parent || cdep.nil?
            tenative_parent = NodeRole.find_by(deployment_id: cdep.id, role_id: parent.id)
            tenative_parent ||= NodeRole.find_by("deployment_id = ? AND role_id in
                                             (select id from roles where ? = ANY(provides))",
                                                 cdep.id,
                                                 parent.name)
            cdep = (cdep.parent rescue nil)
          end
        end
        if tenative_parent
          res << {node_role_id: tenative_parent.id}
        else
          res << find_needed_parents(parent,target_node,target_dep)
          res << {node_id: target_node.id, role_id: parent.id, deployment_id: target_dep.id}
        end
      end
      return res
    end
  end

  def self.bind_needed_parents(parents)
    parents.each do |parent|
      case
      when parent.is_a?(Hash) && parent.has_key?(:node_role_id) then next
      when parent.is_a?(Array) then bind_needed_parents(parent)
      when parent.is_a?(Hash) && parent.has_key?(:role_id) then NodeRole.find_or_create_by!(parent)
      else
        raise "Cannot happen in bind_needed_parents!"
      end
    end
  end

  def self.safe_create!(args)
    r = Role.find_by!(id: args[:role_id])
    n = Node.find_by!(id: args[:node_id])
    d = Deployment.find_by!(id: args[:deployment_id])
    parent_list = find_needed_parents(r,n,d)
    bind_needed_parents(parent_list)
    find_or_create_by!(args)
  end

  def error?
    state == ERROR
  end

  def active?
    state == ACTIVE
  end

  def todo?
    state == TODO
  end

  def transition?
    state == TRANSITION
  end

  def blocked?
    state == BLOCKED
  end

  def proposed?
    state == PROPOSED
  end

  def activatable?
    (parents.count == 0) || (parents.not_in_state(ACTIVE).count == 0)
  end

  def runnable?
    node.available && node.alive && jig.active && committed_data && deployment.committed?
  end

  # convenience methods
  def name
    "#{deployment.name}: #{node.name}: #{role.name}" rescue I18n.t('unknown')
  end

  def deployment_role
    DeploymentRole.find_by(deployment_id: deployment_id,
                           role_id: role_id)
  end

  def deployment_data
    res = {}
    dr = deployment_role
    res.deep_merge!(dr.all_data)
    res.deep_merge!(dr.wall)
    res
  end

  def available
    read_attribute("available")
  end

  def available=(b)
    NodeRole.transaction do
      write_attribute("available",!!b)
      save!
    end
  end

  def update_cohort
    NodeRole.transaction do
      c = (parents.maximum("cohort") || -1)
      if c >= cohort
        update_column(:cohort,  c + 1)
      end
      children.where('cohort <= ?',cohort).each do |child|
        child.update_cohort
      end
    end
  end

  def add_child(new_child, cluster_recurse=true)
    NodeRole.transaction do
      if new_child.is_a?(String)
        new_child = self.node.node_roles.find_by!(role_id: Role.find_by!(name: new_child))
      end
      unless children.any?{|c| c.id == new_child.id}
        children << new_child
        new_child.update_cohort
      end
      # If I am a cluster, then my peers are get my children.
      if self.role.cluster? && cluster_recurse
        NodeRole.peers_by_role(deployment,role).each do |peer|
          next if peer.id == self.id
          peer.add_child(new_child,false)
        end
      end
    end
  end

  def add_parent(new_parent)
    new_parent.add_child(self)
  end

  def data
    proposed? ? proposed_data : committed_data
  end

  def data=(arg)
    raise I18n.t('node_role.cannot_edit_data') unless proposed?
    update!(proposed_data: arg)
  end

  def data_update(val)
    NodeRole.transaction do
      update!(proposed_data: proposed_data.deep_merge(val))
    end
  end

  def sysdata
    return role.sysdata(self) if role.respond_to?(:sysdata)
    read_attribute("sysdata")
  end

  def sysdata=(arg)
    raise("#{role.name} dynamically overwrites sysdata, cannot write to it!") if role.respond_to?(:sysdata)
    NodeRole.transaction do
      update_column("sysdata", arg)
    end
  end

  def sysdata_update(val)
    NodeRole.transaction do
      self.sysdata = self.sysdata.deep_merge(val)
    end
  end

  def wall_update(val)
    NodeRole.transaction do
      self.wall = self.wall.deep_merge(val)
      save!
    end
  end

  def all_my_data
    res = {}
    res.deep_merge!(wall)
    res.deep_merge!(sysdata)
    res.deep_merge!(data)
    res
  end

  def attrib_data
    deployment_data.deep_merge(all_my_data)
  end

  def all_committed_data
    res = deployment_data
    res.deep_merge!(wall)
    res.deep_merge!(sysdata)
    res.deep_merge!(committed_data)
    res
  end

  def all_deployment_data
    res = {}
    all_parents.each {|parent| res.deep_merge!(parent.deployment_data)}
    res.deep_merge(deployment_data)
  end

  def all_parent_data
    res = {}
    all_parents.each do |parent|
      next unless parent.node_id == node_id || parent.role.server
      res.deep_merge!(parent.all_my_data) end
    res
  end

  def all_data
    res = all_deployment_data
    res.deep_merge!(all_parent_data)
    res.deep_merge(all_my_data)
  end

  def all_transition_data
    dres = {}
    sysres = {}
    userres = {}
    NodeRole.transaction(read_only: true) do
      all_parents.include(:deployment_roles, :roles).each do |rent|
        dres.deep_merge!(rent.deployment_data)
        sysres.deep_merge!(rent.sysdata)
        userres.deep_merge!(rent.committed_data)
      end
      dres.deep_merge!(deployment_data)
      dres.deep_merge!(sysdata)
      dres.deep_merge!(committed_data)
    end
    dres.deep_merge(sysres).deep_merge(userres)
  end

  def rerun
    NodeRole.transaction do
      raise InvalidTransition(self,state,TODO,"Cannot rerun transition") unless error?
      write_attribute("state",TODO)
      save!
    end
  end

  def deactivate
    NodeRole.transaction do
      reload
      return if proposed?
      block_or_todo
    end
  end

  def error!
    # We can also go to ERROR pretty much any time.
    # but we silently ignore the transition if in BLOCKED
    NodeRole.transaction do
      reload
      return if blocked?
      update!(state: ERROR)
      # All children of a node_role in ERROR go to BLOCKED.
      all_children.where(["state NOT IN(?,?)",PROPOSED,TRANSITION]).update_all(state: BLOCKED)
    end
  end

  def active!
    # We can only go to ACTIVE from TRANSITION
    # but we silently ignore the transition if in BLOCKED
    NodeRole.transaction do
      update!(run_count: run_count + 1)
      if !node.alive
        block_or_todo
      else
        raise InvalidTransition.new(self,state,ACTIVE) unless transition?
        update!(state: ACTIVE)
      end
    end
    # Moving any BLOCKED noderoles to TODO will be handled in the after_commit hook.
  end

  def todo!
    # You can pretty much always go back to TODO as long as all your parents are ACTIVE
    NodeRole.transaction do
      reload
      raise InvalidTransition.new(self,state,TODO,"Not all parents are ACTIVE") unless activatable?
      update!(state: TODO)
      # Going into TODO transitions any children in ERROR or TODO into BLOCKED
      children.where(["state IN(?,?)",ERROR,TODO]).each do |c|
        c.block!
      end
    end
  end

  def transition!
    # We can only go to TRANSITION from TODO or ACTIVE
    NodeRole.transaction do
      reload
      unless todo? || active? || transition?
        raise InvalidTransition.new(self,state,TRANSITION)
      end
      Rails.logger.info("NodeRole: Transitioning #{name}")
      update!(state: TRANSITION, runlog: "")
    end
  end

  def block!
    # We can pretty much always go to BLOCKED.
    NodeRole.transaction do
      reload
      update!(state: BLOCKED)
      # Going into BLOCKED transitions any children in ERROR or TODO into BLOCKED.
      children.where(["state IN(?,?)",ERROR,TODO]).each do |c|
        c.block!
      end
    end
  end

  def propose!
    # We can also pretty much always go into PROPOSED,
    # and it does not affect the state of our children until
    # we go back out of PROPOSED.
    NodeRole.transaction do
      reload
      update!(state: PROPOSED)
    end
  end

  def name
    "#{deployment.name}: #{node.name}: #{role.name}" rescue I18n.t('unknown')
  end

  # Commit takes us back to TODO or BLOCKED, depending
  def commit!
    NodeRole.transaction do
      reload
      unless proposed?
        raise InvalidTransition.new(self,state,TODO,"Cannot commit! unless proposed")
      end
      return unless proposed? || blocked?
      update!(committed_data: proposed_data)
      block_or_todo
      if !node.alive && node.power[:on]
        node.power.on
      end
      self
    end
  end

  # convenience methods
  def description
    role.description
  end

  def jig
    role.jig
  end

  def rebind_attrib_parents
    NodeRole.transaction do
      role.wanted_attribs.each do |a|
        next unless a.role_id
        target = all_parents.where(role_id: a.role_id).first!
        nra = parent_attrib_links.find_by(attrib: a)
        if nra.nil?
          NodeRoleAttribLink.find_or_create_by!(parent: target, child: self, attrib: a)
        elsif nra.parent != target
          nra.update!(parent: target)
        end
      end
    end
  end

  private

  def block_or_todo
    NodeRole.transaction do
      (activatable? ? todo! : block!)
    end
  end

  def run_hooks
    meth = "on_#{STATES[state]}".to_sym
    if proposed?
      # on_proposed only runs on initial noderole creation.
      Rails.logger.debug("NodeRole #{name}: Calling #{meth} hook.")
      role.send(meth,self)
      return
    end
    return unless previous_changes["state"]
    if deployment.committed? && available &&
        ((!role.destructive) || (run_count == self.active? ? 1 : 0))
      Rails.logger.debug("NodeRole #{name}: Calling #{meth} hook.")
      role.send(meth,self)
    end
    if todo? && runnable? && !Run.exists?(node_id: self.node_id, running: true)
      Rails.logger.info("NodeRole #{name} is runnable, kicking the annealer.")
      Run.run!
    end
    if active?
      NodeRole.transaction do
        # Immediate children of an ACTIVE node go to TODO
        children.where(state: BLOCKED).each do |c|
          Rails.logger.debug("NodeRole #{name}: testing to see if #{c.name} is runnable")
          next unless c.activatable?
          c.todo!
        end
      end
    end
  end

  def poke_attr_dependent_noderoles
    NodeRole.transaction do
      current_data = {}
      previous_data = {}
      [:wall,:sysdata,:committed_data].each do |key|
        current_data.deep_merge!(self.send(key))
        previous_data.deep_merge!(previous_changes[key] ? previous_changes[key][0] : self.send(key))
      end
      # The data we were providing changed, poke any downstream noderoles
      # that get specific data from us.
      if current_data != previous_data
        child_attrib_links.each do |al|
          cnr = al.child
          next unless cnr.runnable? && (cnr.transition? || cnr.active?)
          attr = al.attrib
          next if attr.extract(current_data) == attr.extract(previous_data)
          cnr.send(:block_or_todo)
        end
      end
    end
  end

  def role_is_bindable
    # Check to see if there are any unresolved role_requires.
    # If there are, then this role cannot be bound.
    role = Role.find(role_id)
    unresolved = role.unresolved_requires
    unless unresolved.empty?
      errors.add(:role_id, "role #{role.name} is missing prerequisites: #{unresolved.map{|rr|rr.requires}}")
    end
    # Abstract roles cannot be bound.
    errors.add(:role_id,"role #{role.name} is abstract and cannot be bound to a node") if role.abstract
    # Roles can only be added to a node of their backing jig is active.
    unless role.active?
      # if we are testing, then we're going to just skip adding and keep going
      if Jig.active('test')
        Rails.logger.info("Role: Test mode allows us to coerce role #{name} to use the 'test' jig instead of #{role.jig_name} when it is not active")
        role.jig = Jig.find_by(name: 'test')
        role.save
      else
        errors.add(:role_id, "role '#{role.name}' cannot be bound without '#{role.jig_name}' being active!")
      end
    end
    # Now that we have validated the role side of things, validate the noderole side of things
  end

  def noderole_has_all_parents
    NodeRole.find_needed_parents(role,
                                 Node.find(node_id),
                                 Deployment.find(deployment_id)).each do |e|
      next if e.is_a?(Hash) && e.has_key?(:node_role_id)
      if e.is_a?(Array) && !e.empty?
        errors[:base] << "Next tenative parent also missing prerequisites: #{e.inspect}"
      end
      if e.is_a?(Hash)
        errors[:base] << "Tenative parent noderole #{e.inspect} is not bound!"
      end
    end
  end

  def validate_conflicts
    role = Role.find(role_id)
    Node.find(node_id).node_roles.each do |nr|
      # Test to see if this role conflicts with us, or if we conflict with it.
      if role.conflicts.include?(nr.role.name) || nr.role.conflicts.include?(role.name)
        errors.add(:role, "#{role.name} cannot be bound because it conflicts with previously-bound role #{nr.role.name} on #{node.name}")
      end
      # Test to see if a previously-bound noderole provides this one.
      if nr.role.provides.include?(role.name)
        errors.add(:role, "#{role.name} cannot be bound because it is provided by previously-bound role #{nr.role.name} on #{node.name}")
      end
      # Test to see if we want to provide something that a previously-bound noderole provides.
      if role.provides.include?(nr.role.name)
        errors.add(:role, "#{role.name} cannot be bound because it tries to provide #{nr.role.name}, which is already bound on #{nr.node.name}")
      end
      # Test to see if there are overlapping provides
      overlapping = role.provides & nr.role.provides
      next if overlapping.empty?
      errors.add(:role, "#{role.name} cannot be bound because it and #{nr.role.name} both provide #{overlapping.inspect}")
    end
  end

  def create_deployment_role
    self.role.add_to_deployment(self.deployment)
  end

  def maybe_rebind_attrib_links
    rebind_attrib_parents if deployment_id_changed?
  end

  def bind_needed_parents
    NodeRole.transaction do
      NodeRole.find_needed_parents(role,
                                   Node.find(node_id),
                                   deployment).each do |np|
        unless np.is_a?(Hash) && np.has_key?(:node_role_id)
          myname = self.name
          self.destroy!
          raise "NodeRole: #{myname} missing prequisite #{np.inspect}, despite passing validation!"
        end
        parent = NodeRole.find_by!(id: np[:node_role_id])
        parent.add_child(self)
      end
      rebind_attrib_parents
    end
  end

  def bind_cluster_children
    NodeRole.transaction do
      if self.role.cluster?
        # If I am a cluster role, I also get any children of my peers.
        NodeRole.peers_by_role(deployment,role).each do |peer|
          next if peer.id == self.id
          peer.children.each do |new_child|
            self.add_child(new_child,false)
          end
        end
      end
    end
  end
end

