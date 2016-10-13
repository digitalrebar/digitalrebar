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

class AddPcmsSoft < ActiveRecord::Migration

  # This create a flag in the pcm table to indicate if this is a hard requirement - from a requires add
  # or a soft requirement - from a preceeds add.  The destroy code will use this to determine dependency
  # checking for delete purposes.  Soft dependencies can be removed freely.

  def self.up
    add_column :node_role_pcms, :soft, :boolean, null: false, default: false

    # Create a view that expands all node_role_pcms to include all the
    # recursive parents and children of a node.
    # This is very postgresql 9.3 specific.
    drop_view :node_role_all_pcms
    create_view :node_role_all_pcms, "with recursive p (child_id, parent_id, soft) as (
	select child_id, parent_id, soft from node_role_pcms
	union
	select p.child_id, pcm.parent_id, p.soft from node_role_pcms pcm, p
	where pcm.child_id = p.parent_id)
	select child_id, parent_id, soft from p;"
  end

  def self.down
    # Create a view that expands all node_role_pcms to include all the
    # recursive parents and children of a node.
    # This is very postgresql 9.3 specific.
    drop_view :node_role_all_pcms
    create_view :node_role_all_pcms, "with recursive p (child_id, parent_id) as (
	select child_id, parent_id from node_role_pcms
	union
	select p.child_id, pcm.parent_id from node_role_pcms pcm, p
	where pcm.child_id = p.parent_id)
	select child_id, parent_id from p;"

    remove_column :node_role_pcms, :soft
  end

end


