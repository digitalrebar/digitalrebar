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

class Role < ActiveRecord::Base

  class Role::MISSING_DEP < Exception
  end

  class Role::MISSING_JIG < Exception
  end

  class Role::CONFLICTS < Exception
  end

  validates_uniqueness_of   :name,  :scope => :barclamp_id
  validates_format_of       :name,  :with=>/\A[a-zA-Z][-_a-zA-Z0-9]*\z/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")

  after_commit :resolve_requires_and_jig

  belongs_to      :barclamp
  belongs_to      :jig,               :foreign_key=>:jig_name, :primary_key=>:name
  has_many        :role_requires,     :dependent => :destroy
  has_many        :active_role_requires, -> { where("required_role_id IS NOT NULL") }, class_name: "RoleRequire"
  has_many        :role_requires_children, class_name: "RoleRequire", foreign_key: :requires, primary_key: :name
  has_many        :parents, through: :active_role_requires, source: :parent
  has_many        :children, through: :role_requires_children, source: :role
  has_many        :role_require_attribs, :dependent => :destroy
  has_many        :attribs,           :dependent => :destroy
  has_many        :wanted_attribs,    :through => :role_require_attribs, :class_name => "Attrib", :source => :attrib
  has_many        :node_roles,        :dependent => :destroy
  has_many        :nodes,             :through => :node_roles
  has_many        :deployment_roles,  :dependent => :destroy
  alias_attribute :requires,          :role_requires

  scope           :library,            -> { where(:library=>true) }
  scope           :implicit,           -> { where(:implicit=>true) }
  scope           :discovery,          -> { where(:discovery=>true) }
  scope           :bootstrap,          -> { where(:bootstrap=>true) }
  scope           :abstract,           -> { where(abstract: true)}
  scope           :milestone,          -> { where(milestone: true)}
  scope           :service,            -> { where(service: true)}
  scope           :active,             -> { joins(:jig).where(["jigs.active = ?", true]) }
  scope           :all_cohorts,        -> { active.order("cohort ASC, name ASC") }
  scope           :all_cohorts_desc,   -> { active.order("cohort DESC, name ASC") }

  def unresolved_requires
    RoleRequire.where("role_id in (select role_id from all_role_requires where required_role_id IS NULL AND role_id = ?)",id)
  end

  def all_parents
    Role.where("id in (select required_role_id from all_role_requires where required_role_id IS NOT NULL AND role_id = ?)",id).order("cohort ASC")
  end

  def all_children
    Role.where("id in (select role_id from all_role_requires where required_role_id = ?)",id).order("cohort ASC")
  end

  # incremental update (merges with existing)
  def template_update(val)
    Role.transaction do
      update!(template: template.deep_merge(val))
    end
  end

  # State Transistion Overrides

  def on_error(node_role, *args)
    Rails.logger.debug "No override for #{self.class.to_s}.on_error event: #{node_role.role.name} on #{node_role.node.name}"
  end

  def on_active(node_role, *args)
    Rails.logger.debug "No override for #{self.class.to_s}.on_active event: #{node_role.role.name} on #{node_role.node.name}"
  end

  def on_todo(node_role, *args)
    Rails.logger.debug "No override for #{self.class.to_s}.on_todo event: #{node_role.role.name} on #{node_role.node.name}"
  end

  def on_transition(node_role, *args)
    Rails.logger.debug "No override for #{self.class.to_s}.on_transition event: #{node_role.role.name} on #{node_role.node.name}"
  end

  def on_blocked(node_role, *args)
    Rails.logger.debug "No override for #{self.class.to_s}.on_blocked event: #{node_role.role.name} on #{node_role.node.name}"
  end

  def on_proposed(node_role, *args)
    Rails.logger.debug "No override for #{self.class.to_s}.on_proposed event: #{node_role.role.name} on #{node_role.node.name}"
  end

  # Event triggers for node creation and destruction.
  # roles should override if they want to handle node addition
  def on_node_create(node)
    true
  end

  # Event triggers for node creation and destruction.
  # roles should override if they want to handle node destruction
  def on_node_delete(node)
    true
  end

  # Event hook that will be called every time a node is saved if any attributes changed.
  # Roles that are interested in watching nodes to see what has changed should
  # implement this hook.
  def on_node_change(node)
    true
  end

  # Event hook that is called whenever a new deployment role is bound to a deployment.
  # Roles that need do something on a per-deployment basis should override this
  def on_deployment_create(dr)
    true
  end

  # Event hook that is called whenever a deployment role is deleted from a deployment.
  def on_deployment_delete(dr)
    true
  end

  def noop?
    jig.name.eql? 'noop'
  end

  def name_i18n
    I18n.t(name, :default=>name, :scope=>'common.roles')
  end

  def name_safe
    name_i18n.gsub("-","&#8209;").gsub(" ","&nbsp;")
  end

  def update_cohort
    Role.transaction do
      c = (parents.maximum("cohort") || -1)
      if c >= cohort
        update_column(:cohort,  c + 1)
      end
    end
    children.where('cohort <= ?',cohort).each do |child|
      child.update_cohort
    end
  end

  def depends_on?(other)
    all_parents.exists?(other.id)
  end

  # Make sure there is a deployment role for ourself in the deployment.
  def add_to_deployment(dep)
    DeploymentRole.unsafe_locked_transaction do
      DeploymentRole.find_or_create_by!(role_id: self.id, deployment_id: dep.id)
    end
  end

  def add_to_node(node)
    add_to_node_in_deployment(node,node.deployment)
  end

  # Bind a role to a node in a deployment.
  def add_to_node_in_deployment(node,dep)
    Role.transaction do
      # If we are already bound to this node in a deployment, do nothing.
      res = NodeRole.find_by(node_id: node.id, role_id: self.id)
      return res if res
    end
    Rails.logger.info("Role: Trying to add #{name} to #{node.name}")
    NodeRole.safe_create!(node_id:       node.id,
                          role_id:       id,
                          deployment_id: dep.id)
  end

  def jig
    Jig.find_by(name: jig_name)
  end
  def active?
    j = jig
    return false unless j
    j.active
  end

  def <=>(other)
    return 0 if self.id == other.id
    self.cohort <=> other.cohort
  end

  private

  def resolve_requires_and_jig
    # Find all of the RoleRequires that refer to us,
    # and resolve them.  This will also update the cohorts if needed.
    Role.transaction do
      role_requires_children.where(required_role_id: nil).each do |rr|
        rr.resolve!
      end
      return true unless jig && jig.client_role &&
        !RoleRequire.exists?(role_id: id,
                             requires: jig.client_role_name)
      # If our jig has already been loaded and it has a client role,
      # create a RoleRequire for it.
      RoleRequire.create!(role_id: id,
                          requires: jig.client_role_name)
    end
  end

end
