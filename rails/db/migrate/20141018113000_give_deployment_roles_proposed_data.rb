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

class GiveDeploymentRolesProposedData < ActiveRecord::Migration

  def up
    rename_column :deployment_roles, :data, :committed_data

    change_table :deployment_roles do |t|
      t.json  :proposed_data,  null: true
    end

  end
end
