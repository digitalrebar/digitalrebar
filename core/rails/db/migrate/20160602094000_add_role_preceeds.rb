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

class AddRolePreceeds < ActiveRecord::Migration

  def change
    add_column :roles, :preceeds, :text, array: true, null: false, default: { expr: "ARRAY[]::text[]" }
    create_table :role_preceeds do |t|
      t.belongs_to :role, null: false
      t.text :preceeds, null: false, index: true
      t.integer :child_role_id, foreign_key: { references: :roles, name: "role_preceeds_fk" }
    end
    add_index(:role_preceeds, [:role_id, :preceeds], unique: true)
    
  end
  
end
