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

class AddUtmView < ActiveRecord::Migration

  def up
    create_view :utc_mapping, 'select c.name as capability, utc.user_id as user_id, tp.child_id as tenant_id 
                 from capabilities c, all_tenant_parents tp, user_tenant_capabilities utc
                 where tp.parent_id = utc.tenant_id AND
                       utc.capability_id = c.id
                 union
                 select c.name as capabiliity, utc.user_id as user_id, t.id as tenant_id
                 from capabilities c, tenants t, users u, user_tenant_capabilities utc
                 where t.id = utc.tenant_id AND
                       utc.capability_id = c.id;'
  end

  def down
    drop_view :utc_mapping
  end
end
