# Copyright 2014, Victor Lowther
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

class AddAttribNrInterlinking <ActiveRecord::Migration

  def change
    create_table :node_role_attrib_links do |t|
      t.belongs_to  :parent, null: false, foreign_key: { references: :node_roles, name: 'parent_fk' }, index: true
      t.belongs_to  :child, null: false, foreign_key: { references: :node_roles, name: 'child_fk' }, index: true
      t.belongs_to  :attrib, null: false
      t.timestamps
    end
    add_index(:node_role_attrib_links, [:parent_id, :child_id, :attrib_id], unique: true, name: 'nral_guarantor')
 
    NodeRole.all.each do |nr|
      nr.rebind_attrib_parents
    end
  end
end
