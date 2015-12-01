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

  audited

  after_commit :run_hooks, on: [:update, :create]
  around_save :run_synchronous_hooks
  before_destroy :test_if_destroyable
  validate :role_is_bindable, on: :create
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
  has_many        :parent_attrib_links, class_name: "NodeRoleAttribLink", foreign_key: "child_id", :dependent => :destroy
  has_many        :child_attrib_links,  class_name: "NodeRoleAttribLink", foreign_key: "parent_id", :dependent => :destroy

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

  class InvalidTransition < StandardError
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

  class InvalidState < StandardError
  end

  class MissingJig < StandardError
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

  def as_json(args = nil)
    args ||= {}
    args[:except] = [ :proposed_data, :committed_data, :sysdata, :wall, :notes ]
    args[:methods] = :node_error
    super(args)
  end

  def self.state_name(state)
    raise InvalidState.new("#{state || 'nil'} is not a valid NodeRole state!") unless state and STATES.include? state
    I18n.t(STATES[state], :scope=>'node_role.state')
  end

  def node_error
    return node.state == NodeRole::ERROR
  end

  def self.bind_needed_parents(target_role,target_node,target_dep)
    res = []
    wanted_parents = []
    transaction do
      wanted_parents = Role.find_by_sql "SELECT DISTINCT ON (\"roles\".\"id\") \"roles\".* from \"roles\" where \"roles\".\"id\" in (#{target_role.wanted_attribs.select('role_id').to_sql} UNION #{target_role.parents.select('id').to_sql})"
    end
    wanted_parents.each do |parent|
      tenative_parents = []
      transaction do
        tenative_parents = NodeRole.find_by_sql "SELECT DISTINCT ON (\"node_roles\".\"id\") \"node_roles\".* from \"node_roles\" where \"node_roles\".\"id\" in (#{NodeRole.where(role_id: parent.id, node_id: target_node.id).select('id').to_sql} UNION #{NodeRole.where("node_id = ? AND role_id in (select id from roles where ? = ANY(provides))", target_node.id, parent.name).select('id').to_sql}) LIMIT 1"
        if tenative_parents.empty? && !parent.implicit?
          cdep = target_dep
          while tenative_parents.empty? && cdep
            tenative_parents = NodeRole.find_by_sql "SELECT DISTINCT ON (\"node_roles\".\"id\") \"node_roles\".* from \"node_roles\" where \"node_roles\".\"id\" in (#{NodeRole.where(deployment_id: cdep.id, role_id: parent.id).select('id').to_sql} UNION #{NodeRole.where('deployment_id = ? AND role_id in (select id from roles where ? = ANY(provides))', cdep.id, parent.name).select('id').to_sql}) LIMIT 1"
            break unless tenative_parents.empty?
            cdep = (cdep.parent rescue nil)
          end
        end
      end
      unless tenative_parents.empty?
        Rails.logger.info("NodeRole safe_create: Found parent noderole #{tenative_parents[0].name}")
        res.concat(tenative_parents)
      else
        res << safe_create!(node_id: target_node.id,
                            role_id: parent.id,
                            deployment_id: target_dep.id)
        Rails.logger.info("NodeRole safe_create: Created parent noderole #{res[-1].name}")
      end
      if parent.cluster?
        cluster_peers = NodeRole.find_by_sql(['SELECT * from node_roles WHERE id != ? AND role_id = ? AND deployment_id = ?',
                                              res[-1].id,
                                              res[-1].role_id,
                                              res[-1].deployment_id])
        res.concat(cluster_peers)
      end
    end
    return res
  end

  def self.safe_create!(args)
    res = find_by(args)
    return res if res
    r = Role.find_by!(id: args[:role_id])
    n = Node.find_by!(id: args[:node_id])
    d = Deployment.find_by!(id: args[:deployment_id])
    Rails.logger.info("NodeRole safe_create: Determining parents needed to bind role #{r.name} to node #{n.name} in deployment #{d.name}")
    rents = bind_needed_parents(r,n,d)
    Rails.logger.info("NodeRole safe_create: Binding role #{r.name} to deployment #{d.name}")
    r.add_to_deployment(d)
    Rails.logger.info("NodeRole safe_create: Binding role #{r.name} to node #{n.name} in deployment #{d.name}")
    # Add all our parent/child links in one go.
    NodeRole.locked_transaction do
      res = create!(args)
      query_parts = []
      tenative_cohort = 0
      rents.each do |rent|
        query_parts << "(#{rent.id}, #{res.id})"
        tenative_cohort = rent.cohort + 1 if rent.cohort > tenative_cohort
      end
      unless query_parts.empty?
        ActiveRecord::Base.connection.execute("INSERT INTO node_role_pcms (parent_id, child_id) VALUES #{query_parts.join(', ')}")
      end
      res.cohort = tenative_cohort
      if res.role.cluster?
        cluster_peer_children = NodeRole.where('id in (select distinct child_id from node_role_pcms where parent_id in (select id from node_roles where deployment_id = ? AND role_id = ? AND id != ?))',
                                               res.deployment_id, res.role_id, res.id)
        unless cluster_peer_children.empty?
          query_parts = cluster_peer_children.map{|c|"(#{res.id}, #{c.id})"}.join(', ')
          ActiveRecord::Base.connection.execute("INSERT INTO node_role_pcms (parent_id, child_id) VALUES #{query_parts}")
          cluster_peer_children.each do |c|
            next if c.cohort > res.cohort
            c.update_cohort
          end
        end
      end
      res.save!
      res.rebind_attrib_parents
      r.on_node_bind(res)
    end

    # We only call on_node_change when the node is available to prevent Rebar
    # from noticing changes it should not notice yet.
    # on_node_bind is specific callback for the adding node.  Let the other roles
    # know that the node has changed.
    Role.all_cohorts.each do |r2|
      Rails.logger.debug("Node: Calling #{r2.name} on_node_change for #{n.name}")
      r2.on_node_change(n)
    end if n.available?

    res
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
    node.available && node.alive && jig.active && committed_data &&
      deployment.committed? && !self.proposed? && !self.error?
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
    if self.proposed?
      deployment_role.all_data
    else
      deployment_role.all_committed_data
    end
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
        nc = self.node.node_roles.find_by(role_id: Role.find_by!(name: new_child))
        unless nc
          nc = NodeRole.find_by!("node_id = ? AND role_id in
                                 (select id from roles where ? = ANY(provides))",
                                 self.node.id,
                                 new_child)
        end
        new_child = nc
      end
      unless children.any?{|c| c.id == new_child.id}
        children << new_child
        new_child.update_cohort
      end
      # If I am a cluster, then my peers get my children.
      if self.role.cluster? && cluster_recurse
        NodeRole.peers_by_role(deployment,role).each do |peer|
          next if peer.id == self.id
          peer.add_child(new_child,false)
        end
      end
    end
    new_child.rebind_attrib_parents
    new_child
  end

  def add_parent(new_parent)
    new_parent.add_child(self)
  end

  def note_update(val)
    transaction do
      self.notes = self.notes.deep_merge(val)
      save!
    end
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
    res = read_attribute("sysdata")
    if role.respond_to?(:sysdata)
      res = role.sysdata(self).deep_merge(res)
    end
    res
  end

  def sysdata=(arg)
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

  def attrib_data(only_committed=false)
    deployment_role.all_data(only_committed).deep_merge(only_committed ? all_committed_data : all_my_data)
  end

  def all_committed_data
    res = deployment_role.all_committed_data
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
      if self.proposed?
        res.deep_merge!(parent.all_my_data)
      else
        res.deep_merge!(parent.all_committed_data)
      end
    end
    res
  end

  def all_data
    res = all_deployment_data
    res.deep_merge!(all_parent_data)
    res.deep_merge(all_my_data)
  end

 # Gather all of the attribute data needed for a single noderole run.
  # It should be run to create whatever information will be needed
  # for the actual run before doing the actual run in a delayed job.
  # RETURNS the attribute data needed for a single noderole run.
  def all_transition_data
    res = {}
    # Figure out which attribs will be satisfied from node data vs.
    # which will be satisfied from noderoles.
    NodeRole.transaction do
      node_req_attrs = role.role_require_attribs.select do |rrr|
        attr = rrr.attrib
        raise("RoleRequiresAttrib: Cannot find required attrib #{rrr.attrib_name}") if attr.nil?
        attr.role_id.nil?
      end
      # For all the node attrs, resolve them.  Prefer hints.
      # Start with the node data.
      node_req_attrs.each do |req_attr|
        Rails.logger.info("NodeRole all_transition_data: Adding node attribute #{req_attr.attrib_name} to attribute blob for #{name} run")
        v = req_attr.attrib.extract(node,:all,true)
        res.deep_merge!(v) unless v.nil?
      end
      # Next, do the same for the attribs we want from a noderole.
      parent_attrib_links.each do |al|
        Rails.logger.info("NodeRole all_transition_data: Adding role attribute #{al.attrib.name} from #{al.parent.name}")
        res.deep_merge!(al.attrib.extract(al.parent.deployment_role,:all,true))
        res.deep_merge!(al.attrib.extract(al.parent,:all,true))
      end
      # And all the noderole data from the parent noderoles on this node.
      # This needs to eventaully go away once I figure ot the best way to pull
      # attrib data that hsould always be present on a node.
      all_parents.where(node_id: node.id).each do |pnr|
        res.deep_merge!(pnr.all_committed_data)
      end
      # Add this noderole's attrib data.
      Rails.logger.info("Jig: Merging attribute data from #{name} for jig run.")
      res.deep_merge!(all_committed_data)
      # Make sure we capture default attrib values
      attribs.each do |attr|
        res.deep_merge!(attr.extract(self,:all,true))
      end
      # Add information about the resource reservations this node has in place
      if node.discovery["reservations"]
        res["rebar_wall"] ||= Hash.new
        res["rebar_wall"]["reservations"] = node.discovery["reservations"]
      end
      # Add any hints.
      res["hints"] = node.hint
      # Add quirks
      res["quirks"] = node.quirks
      # And we are done.
    end
    res
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
      if deployment_role.proposed?
        raise InvalidTransition.new(self,state,PROPOSED,"Cannot commit! unless deployment_role committed!")
      end
      role.on_commit(self)
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
        target = all_parents.find_by!(role_id: a.role_id)
        nra = parent_attrib_links.find_by(attrib: a)
        if nra.nil?
          Rails.logger.info("NodeRole rebind_attrib_parents: attrib: #{a.name} Creating parent attrib link for #{self.name} to #{target.name}")
          NodeRoleAttribLink.find_or_create_by!(parent: target, child: self, attrib: a)
        elsif nra.parent != target
          Rails.logger.info("NodeRole rebind_attrib_parents: attrib: #{a.name} Updating parent attrib link for #{self.name} to #{target.name}")
          nra.update!(parent: target)
        end
      end
    end
  end

  private

  def test_if_destroyable
    NodeRole.transaction do
      # If we are a leaf node, we can be destroyed.
      deletable = false
      if children.count == 0
        Rails.logger.info("NodeRole: #{name} has no children, we can delete it.")
        return true
      end
      # If, we have no children not on the same node, or
      # all our children are PROPOSED and have never been run, we can be deleted.
      if all_children.where.not(node_id: node_id).count == 0
        Rails.logger.info("NodeRole: #{name} only has children on the same node, it is deletable")
        deletable = true
      end
      if all_children.where.not(state: PROPOSED, run_count: 0).count == 0
        Rails.logger.info("NodeRole: #{name} only has children that are PROPOSED and have never run, it is deletable")
        deletable = true
      end
      return false unless deletable
      all_children.order("cohort DESC").each do |c|
        c.destroy
      end
      return true
    end
  end

  def block_or_todo
    NodeRole.transaction do
      (activatable? ? todo! : block!)
    end
  end

  def run_synchronous_hooks
    return yield unless changes['state']
    begin
      meth = "sync_on_#{STATES[state]}".to_sym
      Rails.logger.info("NodeRole: calling hook #{meth} for #{name}")
      raise "Hook failed" unless role.send(meth,self)
    rescue StandardError => e
      Rails.logger.fatal("Error #{e.inspect} in NodeRole run hooks!")
      Rails.logger.fatal("Backtrace:\n\t#{e.backtrace.join("\n\t")}")
      self.runlog ||= ""
      self.runlog << "Error handling hook #{meth.to_s}\n"
      self.runlog << "Error #{e.inspect} in NodeRole run_synchronous_hooks!\n"
      self.runlog << "Backtrace:\n\t#{e.backtrace.join("\n\t")}"
      self.state = ERROR
    end
    yield
  end

  def run_hooks
    begin
      meth = "on_#{STATES[state]}".to_sym
      if proposed?
        # on_proposed only runs on initial noderole creation.
        Rails.logger.debug("NodeRole: calling #{meth} hook for #{name}.")
        role.send(meth,self)
        return
      end
      return unless previous_changes['state']
      if deployment.committed? && available &&
         ((!role.destructive) || (run_count == self.active? ? 1 : 0))
        Rails.logger.debug("NodeRole #{name}: Calling #{meth} hook.")
        role.send(meth,self)
      end
      if todo? && runnable?
        Rails.logger.info("NodeRole #{name} is runnable, kicking the annealer.")
        Run.run!
      elsif active?
        node.halt_if_bored(self) if role.powersave
        NodeRole.transaction do
          # Immediate children of an ACTIVE node go to TODO
          children.where(state: BLOCKED).each do |c|
            Rails.logger.debug("NodeRole #{name}: testing to see if #{c.name} is runnable")
            next unless c.activatable?
            c.todo!
          end
        end
        Run.run!
      end
    rescue Exception => e
      Rails.logger.fatal("Error #{e.inspect} in NodeRole run hooks!")
      Rails.logger.fatal("Backtrace:\n\t#{e.backtrace.join("\n\t")}")
      raise e
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

  def maybe_rebind_attrib_links
    rebind_attrib_parents if deployment_id_changed?
  end
end
