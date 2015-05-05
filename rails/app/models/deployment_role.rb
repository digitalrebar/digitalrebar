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

  after_create :role_create_hook
  before_destroy  :role_delete_hook

  belongs_to :deployment
  belongs_to :role
  has_one    :barclamp, :through => :role
  has_many   :attribs, :through => :role

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
        update!(committed_data: proposed_data)
        update!(proposed_data: nil)
        save!
      end
    end
    Publisher.publish_event("deployment_role", "on_change", { :deployment_role => self, :id => self.id })
    # Have any runnable noderoles that use this deployment role rerun.
    deployment.node_roles.where(role_id: role.id).each do |nr|
      nr.todo! if nr.runnable?
    end
  end

  def propose
    update!(proposed_data: committed_data)
    save!
  end

  def all_committed_data
    role.template.deep_merge(self.committed_data).deep_merge(self.wall)
  end

  def all_data
    res = all_committed_data
    res.deep_merge(all_proposed_data) if proposed_data
    res
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
    role.on_deployment_create(self)
    Publisher.publish_event("deployment_role", "on_create", { :deployment_role => self, :id => self.id })
  end

  def role_delete_hook
    role.on_deployment_delete(self)
    Publisher.publish_event("deployment_role", "on_delete", { :deployment_role => self, :id => self.id })
  end

end
