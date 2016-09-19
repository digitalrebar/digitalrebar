# Copyright 2016, RackN
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

class Tenant < ActiveRecord::Base

  after_create      :load_uuid
  after_create      :tenant_root_deployment
  before_destroy    :tenant_remove_deployments

  after_commit :on_create_hooks, on: :create
  after_commit :on_change_hooks, on: :update
  after_commit :on_destroy_hooks, on: :destroy

  belongs_to  :parent,       class_name: "Tenant"
  has_many    :user_tenant_capabilities
  has_many    :users,        -> { distinct }, through: :user_tenant_capabilities
  has_many    :children,     class_name: "Tenant", foreign_key: "parent_id"
  has_many    :deployments
  has_many    :nodes

  def all_children
    Tenant.where(["id IN (select child_id from all_tenant_parents where parent_id = :ten)", {ten: self.id}])
  end

  def self.root
    Tenant.find_by!(:parent_id == nil)
  end

  def root?
    parent_id.nil?
  end

  private

  def load_uuid
    self.reload
  end

  # to manage nodes, tenants need a root deployment
  def tenant_root_deployment
    Tenant.transaction do
      # don't mess with system tenant
      return true if root?
      # move all nodes to the system deployment
      system = Deployment.system
      Deployment.create!(
          name: "tenant-#{name.gsub(" ", "-")}-root", 
          description: "Created by system when tenant #{name} added",
          tenant_id: id,
          parent_id: system.id
          )
    end
  end

  # when a tenant goes away, we should remove all the deployments and reset the nodes back to system
  def tenant_remove_deployments
    Tenant.transaction do
      # don't mess with system tenant
      return true if root?
      root = Tenant.root
      # move all deployments to the root tenant
      deployments.each do |d|
        d.tenant_id = root.id
        d.save!
      end
      # move all nodes to the root tenant
      nodes.each do |n|
        n.tenant_id = root.id
        n.save!
      end
    end
  end

  # Call the on_tenant_delete hooks.
  def on_destroy_hooks
    # do the low cohorts last
    begin
      Event.fire(self, event: 'on_tenant_delete')
    rescue Exception => e
      Rails.logger.error "Tenant: on_tenant_delete #{name} failed with #{e.message}"
    end
  end

  # Call the on_tenant_change hooks.
  def on_change_hooks
    begin
      Event.fire(self, event: 'on_tenant_change')
    rescue Exception => e
      Rails.logger.error "Tenant: on_tenant_change #{name} failed with #{e.message}"
    end
  end

  def on_create_hooks
    # Call all role on_tenant_create hooks with self.
    # These should happen synchronously.
    # do the low cohorts first
    return if @after_create
    @after_create = true
    Event.fire(self, event: 'on_tenant_create')
  end

end

