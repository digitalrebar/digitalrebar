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

class AddEventing < ActiveRecord::Migration

  def self.up
    create_table :events do |t|
      t.uuid        :uuid, nill: false, unique: true, default: 'gen_random_uuid()'
      t.jsonb       :params, null: false
      t.uuid        :event_selectors, array: true, default: []
      t.text        :note
      t.text        :target_class
      t.jsonb       :target
      t.timestamps
    end

    create_table :event_sinks do |t|
      t.uuid   :uuid, null: false, unique: true, default: 'gen_random_uuid()'
      t.text   :endpoint, null: false, unique: true
      t.text   :username
      t.text   :authenticator
      t.text   :notes
      t.timestamps
    end

    create_table :event_selectors do |t|
      t.uuid   :uuid, null: false, unique: true, default: 'gen_random_uuid()'
      t.belongs_to :event_sink, null: false
      t.jsonb :selector, null: false
      t.timestamps
    end
  end

  def self.down
    drop_table :event_sinks
    drop_table :event_selectors
    drop_table :events
  end
end
