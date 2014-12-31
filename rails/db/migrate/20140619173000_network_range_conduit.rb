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

class NetworkRangeConduit < ActiveRecord::Migration

  def change
    change_table :network_ranges do |t|
      t.string       :conduit,    null: true
      t.integer      :vlan,       null: true, default: 0
      t.boolean      :use_vlan,   null: true, default: false
      t.boolean      :use_bridge, null: true, default: false
      t.integer      :team_mode,  null: true, default: 5
      t.boolean      :use_team,   null: true, default: false
    end
  end

end
