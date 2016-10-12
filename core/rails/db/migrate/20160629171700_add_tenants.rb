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

class AddTenants < ActiveRecord::Migration

  def self.up
    create_table :tenants do |t|
      t.uuid        :uuid, nill: false, unique: true, default: 'gen_random_uuid()'
      t.text        :name, null: false, unique: true
      t.text        :description, default: ""
      t.references  :parent
      t.timestamps
    end

    create_table :capabilities do |t|
      t.uuid        :uuid, nill: false, unique: true, default: 'gen_random_uuid()'
      t.text        :name, null: false, unique: true
      t.text        :description, default: ""
      t.text        :source, null: false, default: 'user-defined'
      t.timestamps
    end

    create_table :user_tenant_capabilities do |t|
      t.uuid        :uuid, nill: false, unique: true, default: 'gen_random_uuid()'
      t.belongs_to  :user
      t.belongs_to  :tenant
      t.belongs_to  :capability
      t.timestamps
    end
    add_index(:user_tenant_capabilities, [:user_id, :tenant_id, :capability_id], unique: true, name: 'utc_unique')

    create_view :all_tenant_parents,
                'with recursive d (parent_id, child_id) as (
                   select parent_id, id from tenants where parent_id IS NOT NULL
                     union
                     select dp.parent_id, d.child_id from d, tenants dp
                       where dp.id = d.parent_id )
                 select parent_id, child_id from d where parent_id IS NOT NULL;'
  end

  def self.down
    drop_table :tenants
    drop_table :capabilities
    drop_table :user_tenant_capabilites
    drop_view :all_tenant_parents
  end
end

