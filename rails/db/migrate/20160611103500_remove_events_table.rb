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

class RemoveEventsTable <ActiveRecord::Migration

  def up
    drop_table :events
  end

  def down
    create_table :events do |t|
      t.uuid        :uuid, nill: false, unique: true, default: 'gen_random_uuid()'
      t.jsonb       :params, null: false
      t.uuid        :event_selectors, array: true, default: []
      t.text        :note
      t.text        :target_class
      t.jsonb       :target
      t.timestamps
    end
  end
  
end
