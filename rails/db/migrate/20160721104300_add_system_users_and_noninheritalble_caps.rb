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

class AddSystemUsersAndNoninheritalbleCaps < ActiveRecord::Migration
  def self.up
    # System users are never allowed to login using any method other
    # than via the localhost exemption or from inside the trust zone
    # via X-Authenticated-Username.  In particular, revproxy must fail
    # all authentication attempts from them.
    # Once the system bit is set, it is never allowed to be unset.
    add_column :users, :system, :bool, default: false, null: false

    # UTCs without the inheritable bit are only valid for the tenant they reference,
    # not for that tenant and all subtenants.
    add_column :user_tenant_capabilities, :inheritable, :bool, default: true, null: false

    # Capabilities with an includes list implicitly grant the listed capabilities
    # when granted, recursively.
    add_column :capabilities, :includes, :text, array: true, default: [], null: false

    drop_view :utc_mapping

    create_view :expanded_utcs, 'with recursive sc (capability_id,
                                                    user_id,
                                                    tenant_id,
                                                    inheritable)
                                 as (select capability_id, user_id, tenant_id, inheritable
                                     from user_tenant_capabilities
                                     union
                                     select cc.id,
                                            sc.user_id,
                                            sc.tenant_id,
                                            sc.inheritable
                                     from capabilities cc,
                                          capabilities c,
                                          sc
                                     where sc.capability_id = c.id AND
                                           c.name = ANY (cc.includes))
                                 select * from sc;'

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
                               where atp.parent_id = utc.capability_id AND
                                     utc.capability_id = c.id AND
                                     utc.inheritable;'
  end

  def self.down
    remove_column :users, :system
    remove_column :user_tenant_capabilities, :inheritable
    remove_column :capabilities, :includes

    drop_view :utc_mapping
    drop_view :expanded_utcs

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
end
