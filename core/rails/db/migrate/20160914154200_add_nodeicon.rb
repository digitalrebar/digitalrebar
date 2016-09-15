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

class AddNodeicon < ActiveRecord::Migration

  def self.up
    add_column :nodes, :icon, :text, null: false, default: 'check_circle'
    add_column :roles, :replace_node_icon, :boolean, null: false, default: false
  end

  def self.down
   remove_column :nodes, :icon
   remove_column :roles, :replace_node_icon
  end

end