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

class AddMultitenancy < ActiveRecord::Migration

  def change
    rtm={
      deployments: Deployment,
      dns_name_entries: DnsNameEntry,
      dns_name_filters: DnsNameFilter,
      groups: Group,
      network_allocations: NetworkAllocation,
      network_ranges: NetworkRange,
      network_routers: NetworkRouter,
      networks: Network,
      node_roles: NodeRole,
      nodes: Node,
      providers: Provider,
      users: User,
      event_sinks: EventSink,
      event_selectors: EventSelector
    }
    rtm.each do |table, klass|
      add_column table, :tenant_id , :integer, references: "tenants", null: false

    end

    add_column :users, :current_tenant_id, :integer, null: false, foreign_key: { references: :tenants, name: "current_tenant_fk" }
  end
  
end

