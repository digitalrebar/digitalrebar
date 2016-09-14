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

class UpdateDeploymentParentViewAgain < ActiveRecord::Migration

  def self.up
    drop_view :all_deployment_parents
    create_view :all_deployment_parents,
                'with recursive d (parent_id, child_id) as (
                    select parent_id, id from deployments where parent_id IS NOT NULL
                    union
                    select dp.parent_id, d.child_id from d, deployments dp
                        where dp.id = d.parent_id )
                 select parent_id, child_id from d where parent_id IS NOT NULL;'
  end

  def self.down
    drop_view :all_deployment_parents
    create_view :all_deployment_parents,
                'with recursive d (parent_id, child_id) as (
                    select parent_id, id from deployments
                    union
                    select dp.parent_id, d.child_id from d, deployments dp
                        where dp.id = d.child_id)
                 select parent_id, child_id from d;'
  end
  
end

