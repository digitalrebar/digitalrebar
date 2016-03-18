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

require 'securerandom'

class AddUuidToEverything < ActiveRecord::Migration

  def change
    # We need the pgcrypto extension enabled.
    rtm={
      attribs: Attrib,
      available_hammers: AvailableHammer,
      barclamps: Barclamp,
      deployment_roles: DeploymentRole,
      deployments: Deployment,
      dns_name_entries: DnsNameEntry,
      dns_name_filters: DnsNameFilter,
      groups: Group,
      hammers: Hammer,
      jigs: Jig,
      network_allocations: NetworkAllocation,
      network_ranges: NetworkRange,
      network_routers: NetworkRouter,
      networks: Network,
      node_role_attrib_links: NodeRoleAttribLink,
      node_roles: NodeRole,
      nodes: Node,
      password_change_tokens: PasswordChangeToken,
      providers: Provider,
      role_require_attribs: RoleRequireAttrib,
      role_requires: RoleRequire,
      roles: Role,
      runs: Run,
      users: User
    }
    rtm.each do |table, klass|
      add_column table, :uuid, :uuid, default: 'gen_random_uuid()'
      klass.all.each do |obj|
        obj.uuid = SecureRandom.uuid if obj.uuid.nil?
        obj.save!
      end
      change_column_null table, :uuid, false
      add_index table, :uuid, unique: true
    end
  end
  
end

