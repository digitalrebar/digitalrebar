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

require 'json'

class DeploymentRole < ActiveRecord::Base

  after_create      :load_uuid

  def load_uuid
    self.reload
  end

  private :load_uuid

  after_commit :role_create_hook, on: :create
  before_destroy  :role_delete_hook

  belongs_to :deployment
  belongs_to :role
  has_one    :barclamp, :through => :role
  has_many   :attribs, :through => :role

  scope  :by_name,      -> (n)   { joins(:role).where('roles.name' => n) }

  def as_json(args = nil)
    args ||= {}
    args[:except] = [ :proposed_data, :committed_data, :wall, :notes]
    super(args)
  end

  # convenience methods

  def name
    role.name
  end

  def description
    role.description
  end

  def data
    proposed_data || committed_data
  end

  def data=(val)
    DeploymentRole.transaction do
      # We can always update the data, as it will just kick the
      # deplotment_role back to Proposed.  Ongoing operations will
      # continue to use the previously committed data.
      update!(proposed_data: val)
    end
  end

  def noderoles
    NodeRole.with_role(role).in_deployment(deployment)
  end

  def nodes
    noderoles.map{|nr|nr.node}
  end

  def committed?
    proposed_data.nil?
  end

  def proposed?
    !committed?
  end

  def commit
    return if committed?
    DeploymentRole.transaction do
      if committed_data != proposed_data
        attribs.each do |attr|
          next if attr.get(self,:all,false) == attr.get(self,:all, true)
          noderoles.each do |nr|
            attr.poke(nr)
          end
        end
        update!(committed_data: proposed_data)
      end
      update!(proposed_data: nil)
      save!
    end
  end

  def propose
    update!(proposed_data: committed_data)
    save!
  end

  def all_committed_data
    role.template.deep_merge(self.wall).deep_merge(self.committed_data)
  end

  def all_data(only_committed = false)
    res = all_committed_data
    res.deep_merge(proposed_data) if proposed_data && !only_committed
    res
  end

  def note_update(val)
    transaction do
      self.notes = self.notes.deep_merge(val)
      save!
    end
  end

  def data_update(val)
    DeploymentRole.transaction do
      update!(proposed_data: data.deep_merge(val))
    end
  end

  def wall_update(val)
    DeploymentRole.transaction do
      update!(wall: wall.deep_merge(val))
    end
  end

  private

  def role_create_hook
    return if @created
    @created = true
    Event.fire(self, obj_class: 'role', obj_id: role.name, event: 'on_deployment_create')
  end

  def role_delete_hook
    return false unless noderoles.count == 0
    Event.fire(self, obj_class: 'role', obj_id: role.name, event: 'on_deployment_delete')
  end

end
