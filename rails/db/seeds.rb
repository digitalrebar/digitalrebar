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
# This file should contain all the record creation needed to seed the database with its default values.
# The data can then be loaded with the rake db:seed (or created alongside the db with db:setup).
ActiveRecord::Base.transaction do

  t = Tenant.find_or_create_by!(name: 'system', description: "Global System Tenant")
  t.save!

  # Name, description
  caps = [
    [ "NODE_CREATE",    "Create Nodes" ],
    [ "NODE_READ",      "Read Nodes" ],
    [ "NODE_UPDATE",    "Update Nodes" ],
    [ "NODE_DESTROY",   "Destroy Nodes" ],
    [ "NODE_REDEPLOY",  "Redeploy Nodes" ],
    [ "NODE_SCRUB",     "Scrub Nodes" ],
    [ "NODE_POWER",     "Power actions on nodes" ],
    [ "NODE_COMMIT",    "Commit nodes" ],
    [ "NODE_PROPOSE",   "Propose nodes" ],

    [ "DEPLOYMENT_CREATE",    "Create Deployments" ],
    [ "DEPLOYMENT_READ",      "Read Deployments" ],
    [ "DEPLOYMENT_UPDATE",    "Update Deployments" ],
    [ "DEPLOYMENT_DESTROY",   "Destroy Deployments" ],
    [ "DEPLOYMENT_REDEPLOY",  "Redeploy Deployments" ],
    [ "DEPLOYMENT_COMMIT",    "Commit Deployments" ],
    [ "DEPLOYMENT_PROPOSE",   "Propose Deployments" ],

    [ "CAPABILITY_CREATE",    "Create Capabilities" ],
    [ "CAPABILITY_READ",      "Read Capabilities" ],
    [ "CAPABILITY_UPDATE",    "Update Capabilities" ],
    [ "CAPABILITY_DESTROY",   "Destroy Capabilities" ],

    [ "TENANT_CREATE",    "Create Tenants" ],
    [ "TENANT_READ",      "Read Tenants" ],
    [ "TENANT_UPDATE",    "Update Tenants" ],
    [ "TENANT_DESTROY",   "Destroy Tenants" ],

    [ "TENANT_USER_ADD_CAPABILITIY",   "Add Capability to User in Tenant" ],
    [ "TENANT_USER_DESTROY_CAPABILITIY",   "Remove Capability from User in Tenant" ],

    [ "MACHINE_CREATE",   "Create Machines Only" ],
    [ "MACHINE_ACCOUNT",  "Modify myself only" ]
  ]
  caps.each do |c|
    cap = Capability.find_or_create_by!(name: c[0], description: c[1], source: "rebar-api")
    cap.save!
  end

  u = User.find_or_create_by!(username: 'rebar', is_admin: true, tenant_id: t.id, current_tenant_id: t.id)
  u.digest_password('rebar1')
  u.save!

  caps.each do |c|
    cap = Capability.find_by(name: c[0])
    UserTenantCapability.create!(user_id: u.id, tenant_id: t.id, capability_id: cap.id)
  end

  if Rails.env.development? or Rails.env.test?
    u = User.find_or_create_by!(username: 'developer', is_admin: true, tenant_id: t.id, current_tenant_id: t.id)
    u.digest_password('d1g1t@l')
    u.save!

    caps.each do |c|
      cap = Capability.find_by(name: c[0])
      UserTenantCapability.create!(user_id: u.id, tenant_id: t.id, capability_id: cap.id)
    end
  end

  Nav.find_or_create_by(item: 'root', name: 'nav.root', description: 'nav.root_description', path: "main_app.root_path", order: 0, development: true)

  # monitor
  Nav.find_or_create_by(item: 'monitor', parent_item: 'root', name: 'nav.monitor', description: 'nav.monitor_description', path: "main_app.monitor_path('system')", order: 500)
  Nav.find_or_create_by(item: 'monitor_child', parent_item: 'monitor', name: 'nav.monitor', description: 'nav.monitor_description', path: "main_app.monitor_path('system')", order: 1000)
  Nav.find_or_create_by(item: 'annealer', parent_item: 'monitor', name: 'nav.annealer', description: 'nav.annealer_description', path: "main_app.annealer_path", order: 3000)
  Nav.find_or_create_by(item: 'overview', parent_item: 'monitor', name: 'nav.layercake', description: 'nav.layercake_description', path: "main_app.layercake_path", order: 4000)

  # nodes
  Nav.find_or_create_by(item: 'nodes', parent_item: 'root', name: 'nav.nodes', description: 'nav.nodes_description', path: "main_app.nodes_path", order: 1000)
  Nav.find_or_create_by(item: 'nodes_child', parent_item: 'nodes', name: 'nav.nodes', description: 'nav.nodes_description', path: "main_app.nodes_path", order: 1000)
  Nav.find_or_create_by(item: 'providers', parent_item: 'nodes', name: 'nav.providers', description: 'nav.providers_description', path: "main_app.providers_path", order: 1400)
  Nav.find_or_create_by(item: 'families', parent_item: 'nodes', name: 'nav.families', description: 'nav.families_description', path: "main_app.families_path", order: 1500)
  Nav.find_or_create_by(item: 'groups', parent_item: 'nodes', name: 'nav.groups', description: 'nav.groups_description', path: "main_app.groups_path", order: 2000)
  Nav.find_or_create_by(item: 'getready', parent_item: 'nodes', name: 'nav.getready', description: 'nav.getready_description', path: "main_app.getready_path", order: 3000)
  Nav.find_or_create_by(item: 'bulk_edit', parent_item: 'nodes', name: 'nav.bulk_edit', description: 'nav.bulk_edit_description', path: "main_app.bulk_edit_path", order: 4000)


  # deployments
  Nav.find_or_create_by(item: 'deploy', parent_item: 'root', name: 'nav.deployments', description: 'nav.deployments_description', path: "main_app.deployments_path", order: 2000)
  Nav.find_or_create_by(item: 'deploy_child', parent_item: 'deploy', name: 'nav.deployments', description: 'nav.deployments_description', path: "main_app.deployments_path", order: 1000)
  Nav.find_or_create_by(item: 'roles', parent_item: 'deploy', name: 'nav.roles', description: 'nav.roles_description', path: "main_app.roles_path", order: 2000)

  # utils
  Nav.find_or_create_by(item: 'utils', parent_item: 'root', name: 'nav.utils', description: 'nav.utils_description', path: "main_app.utils_path", order: 6000)
  Nav.find_or_create_by(item: 'manage_users', parent_item: 'utils', name: 'nav.manage_users', description: 'nav.manage_users_description', path: "main_app.users_path", order: 100)
  Nav.find_or_create_by(item: 'manage_tenants', parent_item: 'utils', name: 'nav.manage_tenants', description: 'nav.manage_tenants_description', path: "main_app.tenants_path", order: 120)
  Nav.find_or_create_by(item: 'manage_capabilities', parent_item: 'utils', name: 'nav.manage_capabilities', description: 'nav.manage_capabilities_description', path: "main_app.capabilities_path", order: 140)
  Nav.find_or_create_by(item: 'user_settings', parent_item: 'utils', name: 'nav.user_settings', description: 'nav.user_settings_description', path: "main_app.utils_settings_path", order: 200)
  Nav.find_or_create_by(item: 'jigs', parent_item: 'utils', name: 'nav.jigs', description: 'nav.jigs_description', path: "main_app.jigs_path", order: 300)
  Nav.find_or_create_by(item: 'hammers', parent_item: 'utils', name: 'nav.hammers', description: 'nav.hammers_description', path: "main_app.available_hammers_path", order: 1000)
  Nav.find_or_create_by(item: 'barclamps', parent_item: 'utils', name: 'nav.barclamps', description: 'nav.barclamps_description', path: "main_app.barclamps_path", order: 4000)

  # networks
  Nav.find_or_create_by(item: 'networks', parent_item: 'root', name: 'nav.networks', description: 'nav.networks_description', path: "networks_path", order: 1500)
  Nav.find_or_create_by(item: 'networks_child', parent_item: 'networks', name: 'nav.networks', description: 'nav.networks_description', path: "networks_path", order: 1000)
  Nav.find_or_create_by(item: 'network_map', parent_item: 'networks', name: 'nav.network_map', description: 'nav.network_map_description', path: "network_map_path", order: 5000)
  Nav.find_or_create_by(item: 'interfaces', parent_item: 'networks', name: 'nav.interfaces', description: 'nav.interfaces_description', path: "interfaces_path", order: 5000)
  Nav.find_or_create_by(item: 'filters', parent_item: 'networks', name: 'nav.filters', description: 'nav.dns_name_filters_description', path: "dns_name_filters_path", order: 2000)
  Nav.find_or_create_by(item: 'names', parent_item: 'networks', name: 'nav.names', description: 'nav.dns_name_entries_description', path: "dns_name_entries_path", order: 2500)

end

Dir.glob("/opt/digitalrebar/**/rebar_engine/barclamp_*/db/seeds.rb") do |seedfile|
  ActiveRecord::Base.transaction do
    "#{seedfile.split('/')[-3].camelize}::Engine".constantize.load_seed
  end
end
