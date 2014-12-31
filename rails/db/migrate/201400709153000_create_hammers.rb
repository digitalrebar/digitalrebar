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

class CreateHammers < ActiveRecord::Migration
  def change

    create_table :available_hammers do |t|
      t.string     :name,               null: false, index: :unique
      t.string     :klass,              null: false, index: :unique
      t.integer    :priority,           null: false
      t.timestamps
    end

    create_table :hammers do |t|
      t.belongs_to :node,               null: false
      t.belongs_to :available_hammer, null: false
      t.integer    :priority,           null: false, default: 0
      t.string     :type,               null: false, foreign_key: { references: [:available_hammers, :klass] }
      t.string     :name,               null: false, foreign_key: { references: [:available_hammers, :name] }
      # How the node manager should talk to the node being managed.
      # How this is parsed is hammer dependent.
      t.text       :endpoint,           null: true
      # The username that the manager should use for this node.
      t.string     :username,           null: false
      # The authenticating token that the manager should use for this node.
      # It could be a password, it could be half of a keypair, or it
      # could be something entirely different.
      t.text       :authenticator,      null: true
    end
    # Only one manager of a given type can be bound to a node.
    add_index(:hammers, [:node_id, :type], unique: true)
  end
end
