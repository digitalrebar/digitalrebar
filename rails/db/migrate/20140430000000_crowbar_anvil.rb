# Copyright 2014, Rob Hirschfeld
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
class CrowbarAnvil < ActiveRecord::Migration

  def self.up

    create_table :docs do |t|
      t.text        :name,        index: { unique: true }
      t.belongs_to  :barclamp,    null: true, foreign_key: false
      t.text        :description, null: true
      t.belongs_to  :parent,      null: true
      t.string      :order,       length: 5, default: '009999'
      t.timestamps
    end

    create_table(:navs, :primary_key=>'name', :id=>false) do |t|
      t.string :item
      t.string :parent_item, :default=>'root'
      t.string :name
      t.string :description
      t.string :path
      t.integer :order, :length=>5, :default => 9999
      t.boolean :development, :default=>false
      t.timestamps
    end

    create_table :jigs do |t|
      t.string      :name,             index:   { unique: true }
      t.string      :description,      null:    true
      t.integer     :order,            default: 10000
      t.string      :type,             null:    false
      t.boolean     :active,           default: false
      t.string      :client_role_name, null:    true
      t.string      :server,           null:    true
      t.string      :client_name,      null:    true
      t.text        :key,              null:    true
      t.timestamps
    end

    create_table(:users) do |t|
      ## Database authenticatable
      t.string :email,              null: false, default: "", index: true
      t.string :encrypted_password, null: false, default: ""

      # crowbar additions
      t.string :username,           null: false, default: "", index: { unique: true }
      t.boolean :is_admin,          default: false, null: false

      ## Recoverable
      t.string   :reset_password_token, index: { unique: true }
      t.datetime :reset_password_sent_at

      ## Rememberable
      t.datetime :remember_created_at

      ## Trackable
      t.integer  :sign_in_count, default: 0
      t.datetime :current_sign_in_at
      t.datetime :last_sign_in_at
      t.string   :current_sign_in_ip
      t.string   :last_sign_in_ip

      ## Confirmable
      t.string   :confirmation_token, index: { unique: true }
      t.datetime :confirmed_at
      t.datetime :confirmation_sent_at
      t.string   :unconfirmed_email # Only if using reconfirmable

      ## Lockable
      t.integer  :failed_attempts, default: 0 # Only if lock strategy is :failed_attempts
      t.string   :unlock_token, index: { unique: true }  # Only if unlock strategy is :email or :both
      t.datetime :locked_at

      ## Token authenticatable
      # t.string :authentication_token
      t.timestamps
    end

    create_table :barclamps do |t|
      t.string      :name,        index: { unique: true }
      t.string      :description, null: true
      t.belongs_to  :barclamp,    null: true
      t.integer     :version
      t.string      :source_url,  null: true
      t.string      :source_path, null: true
      t.string      :commit,      null: true, default: 'unknown'
      t.datetime    :build_on,    null: true, default: Time.now
      t.timestamps
    end
    # Satisfy docs dependency on barclamps fk.
    add_foreign_key('docs', 'barclamp_id', 'barclamps','id')

    create_table :roles do |t|
      t.string      :name,              null: false, index: { unique: true }
      t.string      :description,       null: true
      t.string      :type,              null: true
      t.json        :template,          null: false, default: { expr: "'{}'::json" }
      t.string      :jig_name,          null: false
      t.text        :provides,          array: true, null: false, default: { expr: "ARRAY[]::text[]" }
      t.text        :conflicts,         array: true, null: false, default: { expr: "ARRAY[]::text[]" }
      # brings in library code thats used to access another role (sql client)
      t.boolean     :library,           null: false, default: false
      # role is applied automatically to nodes after allocation
      t.boolean     :implicit,          null: false, default: false
      # used for the admin node(s) during bring up
      t.boolean     :bootstrap,         null: false, default: false
      # related to the node being discovered in the system (by deployer)
      t.boolean     :discovery,         null: false, default: false
      # Indicates that this role implements a clustered service.
      # When the noderole graph is built, any child noderoles of this service
      # will be bound to all of the noderoled for this role in the deployment.
      # The cluster flag and the implicit flag are mutually exclusive.
      t.boolean     :cluster,           null: false, default: false
      # Indicates that the role is not idempotent -- once a noderole binding
      # it has transitioned to active, it will not be run again.
      t.boolean     :destructive,       null: false, default: false
      # Indicates that the role cannot be bound to a node -- instead, it exists only to provide
      # its attributes to other roles.
      t.boolean     :abstract,          null: false, default: false
      t.belongs_to  :barclamp,          null: false
      t.integer     :cohort,            null: false, default:0
      t.timestamps
    end

    create_table :deployments do |t|
      t.integer     :state,       null: false, default: Deployment::PROPOSED
      t.string      :name,        null: false, index: :unique
      t.string      :description, null: true
      t.boolean     :system,      null: false, default: false
      t.references  :parent
      t.timestamps
    end
    

    create_table :role_requires do |t|
      t.belongs_to  :role,            null: false
      t.string      :requires,        null: false, index: true
      t.integer     :required_role_id, foreign_key: { references: :roles, name: "role_requires_fk" }
      t.timestamps
    end
    add_index(:role_requires, [:role_id, :requires], unique: true)

    create_table :role_require_attribs do |t|
      t.belongs_to  :role
      t.string      :attrib_name
      t.string      :attrib_at
      t.timestamps
    end
    add_index(:role_require_attribs, [:role_id, :attrib_name], :unique => true)
    # Create a view that allows us to recursively get all the parents
    # and children of a given role.  Holes in the graph will have NULL
    # in place of parent_id.

    create_view :all_role_requires, "
with recursive arr (role_id, required_role_id, required_role_name) as (
    select role_id, required_role_id, requires from role_requires
    union
    select arr.role_id, rr.required_role_id, rr.requires
    from arr, role_requires rr
    where rr.requires  = arr.required_role_name)
    select role_id, required_role_id, required_role_name from arr;"

    create_view :all_role_requires_paths, "
with recursive arrp (child_name, parent_name, path) as (
    select r.name, rr.requires, ARRAY[r.name::text,rr.requires::text]
    from role_requires rr, roles r where r.id = rr.role_id
    union
    select arrp.child_name, rr.requires, arrp.path || rr.requires::text
    from arrp, roles r, role_requires rr
    where r.id = rr.role_id AND r.name = arrp.parent_name)
    select child_name, parent_name, path from arrp;"

    create_table :nodes do |t|
      t.string      :name,           limit: 255,     null: false
      t.string      :alias,          limit: 100,     null: false
      t.string      :description,    null: true
      t.integer     :order,          default: 10000
      t.boolean     :admin,          default: false
      t.integer     :target_role_id, null: true, foreign_key: { references: :roles }
      t.belongs_to  :deployment,     null: false
      t.json        :discovery,      null: false,   default: { expr: "'{}'::json" }
      t.json        :hint,           null: false,   default: { expr: "'{}'::json" }
      t.boolean     :allocated,      null: false,   default: false
      t.boolean     :alive,          null: false,   default: false
      t.boolean     :available,      null: false,   default: false
      t.string      :bootenv,        null: false,   default: "sledgehammer"
      t.timestamps
    end
    #natural key
    add_index(:nodes, :name, :unique => true)

    create_table :attribs do |t|
      t.belongs_to  :barclamp,      null: true, index: true
      t.belongs_to  :role,          null: true, index: true
      t.string      :type,          null: true
      t.string      :name,          null: false, index: { unique: true }
      t.string      :description,   null: true
      t.boolean     :writable,      null: false, default: false
      t.text        :schema
      t.integer     :order,         default: 10000
      t.string      :map,           null: true
      t.string      :ui_renderer,   null: false, default: "attribs/default"
      t.timestamps
    end

    create_table :deployment_roles do |t|
      t.belongs_to  :deployment, null: false
      t.belongs_to  :role,       null: false
      t.json        :data,       null: false, default: { expr: "'{}'::json" }
      t.json        :wall,       null: false, default: { expr: "'{}'::json" }
      t.timestamps
    end
    #natural key
    add_index(:deployment_roles, [:deployment_id, :role_id], :unique => true)
    add_index(:deployment_roles, :role_id)

    create_table :node_roles do |t|
      t.belongs_to  :deployment,        null: false, index: true
      t.belongs_to  :role,              null: false, index: true
      t.belongs_to  :node,              null: false, index: true
      t.integer     :state,             null: false, default: NodeRole::PROPOSED
      t.integer     :cohort,            null: false, default: 0
      t.integer     :run_count,         null: false, default: 0
      t.string      :status,            null: true   # expected for error, blocked, transistioning
      t.text        :runlog,            null: false, default: ""
      t.boolean     :available,         null: false, default: true
      t.integer     :order,             default: 10000
      t.json        :proposed_data,     null: false, default: { expr: "'{}'::json" }
      t.json        :committed_data,    null: true
      t.json        :sysdata,           null: false, default: { expr: "'{}'::json" }
      t.json        :wall,              null: false, default: { expr: "'{}'::json" }
      t.timestamps
    end
    #natural key
    add_index(:node_roles, [:role_id, :node_id], unique: true)

    create_table :node_role_pcms, id: false do |t|
      t.integer     :parent_id, foreign_key: { references: :node_roles, name: 'parent_fk' }, index: true
      t.integer     :child_id, foreign_key: { references: :node_roles, name: 'child_fk' }, index: true
    end
    add_index(:node_role_pcms, [:parent_id, :child_id], unique: true)

    # Create a view that expands all node_role_pcms to include all the
    # recursive parents and children of a node.
    # This is very postgresql 9.3 specific.
    create_view :node_role_all_pcms, "
with recursive p (child_id, parent_id) as (
      select child_id, parent_id from node_role_pcms
      union
      select p.child_id, pcm.parent_id from node_role_pcms pcm, p
      where pcm.child_id = p.parent_id)
      select child_id, parent_id from p;"

    create_table :groups do |t|
      t.string      :name,        index: { unique: true }
      t.string      :description, null: true
      t.string      :category,    default: 'ui'
      t.integer     :order,       default: 10000
      t.timestamps
    end
    #natural key
    add_index(:groups, [:category, :name], unique: true)

    create_table :node_groups, id: false do |t|
      t.belongs_to  :node
      t.belongs_to  :group
    end
    #natural key
    add_index(:node_groups, [:node_id, :group_id], unique: true)

   create_table "networks" do |t|
      t.references   :deployment
      t.string       :name,       null: false, index: { unique: true }
      t.string       :description,null: true
      t.integer      :order,      null: false, default: 1000
      t.integer      :vlan,       null: false, default: 0
      t.boolean      :use_vlan,   null: false, default: false
      t.boolean      :use_bridge, null: false, default: false
      t.integer      :team_mode,  null: false, default: 5
      t.boolean      :use_team,   null: false, default: false
      t.string       :v6prefix
      # This contains abstract interface names seperated by a comma.
      # It could be normalized, but why bother for now.
      t.string       :conduit,    null: false
      t.timestamps
    end

    create_table "network_routers" do |t|
      t.references   :network
      t.string       :address,    null: false
      t.integer      :pref,       null: false, default: 65536
      t.timestamps
    end

    create_table "network_ranges" do |t|
      t.string       :name,       null: false
      t.references   :network
      # Both of these should also be CIDRs.
      t.string       :first,      null: false
      t.string       :last,       null: false
      t.timestamps
    end
    add_index "network_ranges", [:network_id, :name], unique: true

    create_table "network_allocations" do |t|
      t.references   :node
      t.references   :network
      t.references   :network_range
      t.string       :address,    null: false, index: { unique: true }
      t.timestamps
    end

    create_view :dns_database, "select n.name as name,
                                       n.alias as cname,
                                       a.address as address,
                                       net.name as network
                                from nodes n
                                inner join network_allocations a on n.id = a.node_id
                                inner join networks net on net.id = a.network_id"

    create_view :dhcp_database, "select n.name as name,
                                        n.bootenv as bootenv,
                                        a.address as address,
                                        json_extract_path(n.discovery,'ohai','network','interfaces') as discovered_macs,
                                        json_extract_path(n.hint,'admin_macs') as hinted_macs
                                 from nodes n
                                 inner join network_allocations a on a.node_id = n.id
                                                                  and family(a.address::inet) = 4
                                 inner join networks net on a.network_id = net.id and net.name = 'admin'
                                 inner join node_roles nr on nr.node_id = n.id
                                 inner join roles r on nr.role_id = r.id and r.name = 'crowbar-managed-node'
                                 where json_extract_path(n.hint,'admin_macs') is not null
                                 or json_extract_path(n.discovery,'ohai','network','interfaces') is not null"

    create_view :docker_database, "select n.name as name,
                                       a.address as address
                                from nodes n
                                inner join network_allocations a on n.id = a.node_id
                                inner join networks net on net.id = a.network_id
                                inner join node_roles nr on nr.node_id = n.id
                                inner join roles r on nr.role_id = r.id
                                where net.name = 'admin'
                                and r.name = 'crowbar-docker-node'"

    create_table :runs do |t|
      t.belongs_to  :node_role,        null: false, index: true
      t.belongs_to  :node,             null: false, index: true
      t.text        :run_data,         null: false
      t.boolean     :running,          null: false, default: false, index: true
    end

    create_table :delayed_jobs, :force => true do |table|
      table.integer  :priority, :default => 0, :null => false # Allows some jobs to jump to the front of the queue
      table.integer  :attempts, :default => 0, :null => false # Provides for retries, but still fail eventually.
      table.text     :handler, :null => false                 # YAML-encoded string of the object that will do work
      table.text     :last_error                              # reason for last failure (See Note below)
      table.datetime :run_at                                  # When to run. Could be Time.zone.now for immediately, or sometime in the future.
      table.datetime :locked_at                               # Set when a client is working on this object
      table.datetime :failed_at                               # Set when all retries have failed (actually, by default, the record is deleted instead)
      table.string   :locked_by                               # Who is working on this object (if locked)
      table.string   :queue                                   # The name of the queue this job is in
      table.timestamps
    end

    add_index :delayed_jobs, [:priority, :run_at], :name => 'delayed_jobs_priority'

  end

end
