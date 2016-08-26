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

class FixUtcTenantId < ActiveRecord::Migration
  def self.up
    drop_view :utc_mapping
    create_view :utc_mapping, 'select c.name as capability,
                                      utc.user_id as user_id,
                                      utc.tenant_id as tenant_id
                               from capabilities c, expanded_utcs utc
                               where c.id = utc.capability_id
                               union
                               select c.name as capability,
                                      utc.user_id as user_id,
                                      atp.child_id as tenant_id
                               from capabilities c,
                                    all_tenant_parents atp,
                                    expanded_utcs utc
                               where atp.parent_id = utc.tenant_id AND
                                     utc.capability_id = c.id AND
                                     utc.inheritable;'
  end
end
