# Copyright 2015, RackN
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
class DrillConsolidated < ActiveRecord::Migration

  def self.up

    # The current version as of this migration's creation.
    Que.migrate! :version => 3  # 20150513214112 

    # 20140122123641
    create_table :settings do |t|
      t.string     :var,    null: false
      t.text       :value,  null: false, default: '--- {}'
      t.references :target, null: false, polymorphic: true
      t.timestamps
    end
    add_index :settings, [ :target_type, :target_id, :var ], :unique => true

    create_table(:navs, :primary_key=>'name', :id=>false) do |t|
      t.text      :item
      t.text      :parent_item,   default: 'root'
      t.text      :name
      t.text      :description,   default: ''
      t.text      :path
      t.integer   :order,         length: 5, default: 9999
      t.boolean   :development,   default: false
      t.timestamps
    end

    create_table :jigs do |t|
      t.text        :name,             null: false, index:   { unique: true }
      t.text        :description,      null: false, default: ''
      t.integer     :order,            null: false, default: 10000
      t.text        :type,             null: false
      t.boolean     :active,           null: false, default: false
      t.text        :client_role_name, null: true
      t.text        :server,           null: true
      t.text        :client_name,      null: true
      t.text        :key,              null: true
      t.timestamps
    end

    create_table(:users) do |t|
      ## Database authenticatable
      t.text   :email,              null: false, default: "", index: true
      t.text   :encrypted_password, null: false, default: ""

      # rebar additions
      t.text   :username,           null: false, default: "", index: { unique: true }
      t.boolean :is_admin,          default: false, null: false

      ## Recoverable
      t.text     :reset_password_token, index: { unique: true }
      t.datetime :reset_password_sent_at

      ## Rememberable
      t.datetime :remember_created_at

      ## Trackable
      t.integer  :sign_in_count, default: 0
      t.datetime :current_sign_in_at
      t.datetime :last_sign_in_at
      t.text     :current_sign_in_ip
      t.text     :last_sign_in_ip

      ## Confirmable
      t.text     :confirmation_token, index: { unique: true }
      t.datetime :confirmed_at
      t.datetime :confirmation_sent_at
      t.text     :unconfirmed_email # Only if using reconfirmable

      ## Lockable
      t.integer  :failed_attempts, default: 0 # Only if lock strategy is :failed_attempts
      t.text     :unlock_token, index: { unique: true }  # Only if unlock strategy is :email or :both
      t.datetime :locked_at

      t.timestamps
    end

    # 20150812142500
    create_table :password_change_tokens do |t|
      t.belongs_to :user
      t.text :key, null: false, index: true
      t.text :token, null: false
      t.timestamps
    end

    create_table :barclamps do |t|
      t.text        :name,        null: false, default: '', index: { unique: true }
      t.text        :description, null: false, default: ''
      t.belongs_to  :barclamp,    null: true
      t.integer     :version
      t.text        :source_url,  null: true
      t.text        :source_path, null: true
      t.text        :commit,      null: true, default: 'unknown'
      t.datetime    :build_on,    null: true, default: Time.now
      t.timestamps
      t.text        :cfg_data,    null: false    # 20140627064000
      t.text        :build_version, null: true, default: 'unknown' # 20141113135600
    end

    create_table :roles do |t|
      t.text        :name,              null: false, index: { unique: true }
      t.text        :description,       null: false, default: ''
      t.text        :type,              null: true
      t.jsonb       :template,          null: false, default: { expr: "'{}'::jsonb" }
      t.text        :jig_name,          null: false
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
      t.boolean     :milestone,         null: false, default: false # 2014062400
      t.boolean     :powersave,         null: false, default: false # 20150109193000
      t.boolean     :service,           null: false, default: false # 20150203155600
      t.jsonb       :notes,             null: false, default: { expr: "'{}'::jsonb" }  # 20150713092600
      t.text        :icon,              null: false, default: 'memory'  # 20160117
      t.jsonb       :metadata,          null: false, default: { expr: "'{}'::jsonb" }  # 20160123104600
    end

    create_table :deployments do |t|
      t.integer     :state,       null: false, default: Deployment::PROPOSED
      t.text        :name,        null: false, index: :unique
      t.text        :description, null: false, default: ''
      t.boolean     :system,      null: false, default: false
      t.references  :parent
      t.timestamps
    end

    create_table :role_requires do |t|
      t.belongs_to  :role,            null: false
      t.text        :requires,        null: false, index: true
      t.integer     :required_role_id, foreign_key: { references: :roles, name: "role_requires_fk" }
      t.timestamps
    end
    add_index(:role_requires, [:role_id, :requires], unique: true)

    create_table :role_require_attribs do |t|
      t.belongs_to  :role
      t.text        :attrib_name
      t.text        :attrib_at
      t.timestamps
    end
    add_index(:role_require_attribs, [:role_id, :attrib_name], :unique => true)

    create_table :providers do |t|
      t.text        :name,           null: false, index: :unique
      t.text        :type
      t.text        :description,    null: false, default: "An undescribed provider"
      t.jsonb       :auth_details,   null: false, default: { expr: "'{}'::jsonb" }
      t.timestamps
    end

    create_table :nodes do |t|
      t.text        :name,           limit: 255,     null: false
      t.text        :description,    null: false, default: ''
      t.integer     :order,          null: false, default: 10000
      t.boolean     :admin,          default: false
      t.integer     :target_role_id, null: true, foreign_key: { references: :roles }
      t.belongs_to  :deployment,     null: false
      t.belongs_to  :provider,       null: false
      t.text        :variant,        null: false,   default: "metal"
      t.text        :arch,           null: false,   default: "x86_64"
      t.text        :os_family,      null: false,   default: "linux"
      t.jsonb        :discovery,      null: false,   default: { expr: "'{}'::jsonb" }
      t.jsonb        :hint,           null: false,   default: { expr: "'{}'::jsonb" }
      t.boolean     :allocated,      null: false,   default: false
      t.boolean     :alive,          null: false,   default: false
      t.boolean     :available,      null: false,   default: false
      t.text        :bootenv,        null: false,   default: "sledgehammer"
      t.timestamps
      t.text        :quirks,         array: true,   null: false, default: []       # 2014082713150
      t.boolean     :system,         null: false,   default: false       # 20150104222800
      t.jsonb        :notes,          null: false,   default: { expr: "'{}'::jsonb" }  # 20150713092600
    end
    #natural key
    add_index(:nodes, :name, :unique => true)

    create_table :attribs do |t|
      t.belongs_to  :barclamp,      null: true, index: true
      t.belongs_to  :role,          null: true, index: true
      t.text        :type,          null: true
      t.text        :name,          null: false, index: { unique: true }
      t.text        :description,   null: false, default: ''
      t.boolean     :writable,      null: false, default: false
      t.text        :schema
      t.integer     :order,         default: 10000
      t.text        :map,           null: false, default: ''
      t.text        :ui_renderer,   null: false, default: "attribs/default"
      t.timestamps
      t.jsonb        :default,       default: { expr: "'{\"value\": null}'::jsonb" }  # 20150516140700
    end

    create_table :deployment_roles do |t|
      t.belongs_to  :deployment,    null: false
      t.belongs_to  :role,          null: false
      t.jsonb        :committed_data, null: false, default: { expr: "'{}'::jsonb" }
      t.jsonb        :wall,          null: false, default: { expr: "'{}'::jsonb" }
      t.timestamps
      t.jsonb        :proposed_data, null: true       # 2014101811300
      t.jsonb        :notes,          null: false,   default: { expr: "'{}'::jsonb" }  # 20150713092600
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
      t.text        :status,            null: false, default: ''   # expected for error, blocked, transistioning
      t.text        :runlog,            null: false, default: ""
      t.boolean     :available,         null: false, default: true
      t.integer     :order,             null: false, default: 10000
      t.jsonb        :proposed_data,     null: false, default: { expr: "'{}'::jsonb" }
      t.jsonb        :committed_data,    null: false, default: { expr: "'{}'::jsonb" }
      t.jsonb        :sysdata,           null: false, default: { expr: "'{}'::jsonb" }
      t.jsonb        :wall,              null: false, default: { expr: "'{}'::jsonb" }
      t.timestamps
      t.jsonb        :notes,             null: false,   default: { expr: "'{}'::jsonb" }  # 20150713092600
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
    create_view :node_role_all_pcms, "with recursive p (child_id, parent_id) as (
              select child_id, parent_id from node_role_pcms
              union
              select p.child_id, pcm.parent_id from node_role_pcms pcm, p
              where pcm.child_id = p.parent_id)
              select child_id, parent_id from p;"

    # 2014101519000
    create_table :node_role_attrib_links do |t|
      t.belongs_to  :parent, null: false, foreign_key: { references: :node_roles, name: 'parent_fk' }, index: true
      t.belongs_to  :child, null: false, foreign_key: { references: :node_roles, name: 'child_fk' }, index: true
      t.belongs_to  :attrib, null: false
      t.timestamps
    end
    add_index(:node_role_attrib_links, [:parent_id, :child_id, :attrib_id], unique: true, name: 'nral_guarantor')

    create_table :groups do |t|
      t.text        :name,        null: false
      t.text        :description, null: false, default: ''
      t.text        :category,    null: false, default: 'ui'
      t.integer     :order,       null: false, default: 10000
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
      t.references   :deployment, null: false
      t.text         :name,       null: false, index: { unique: true }
      t.text         :description,null: false, default: ''
      t.integer      :order,      null: false, default: 1000
      t.integer      :vlan,       null: false, default: 0
      t.boolean      :use_vlan,   null: false, default: false
      t.boolean      :use_bridge, null: false, default: false
      t.integer      :team_mode,  null: false, default: 5
      t.boolean      :use_team,   null: false, default: false
      t.text         :v6prefix
      # This contains abstract interface names seperated by a comma.
      # It could be normalized, but why bother for now.
      t.text         :conduit,    null: false
      t.timestamps
      t.boolean     :configure,   null: false, default: true   # 20150325101300
      t.text        :category,    null: false, default: "general"   # 20150327154100
      t.text        :group,       null: false, default: 'default'  # 20150418154100
      t.text        :pbr,         null: true      # 2015070101600
    end
    # required constraint
    add_index(:networks, [:category, :group], unique: true)

    create_table "network_routers" do |t|
      t.references   :network,    null: false
      t.text         :address,    null: false
      t.integer      :pref,       null: false, default: 65536
      t.timestamps
    end

    create_table "network_ranges" do |t|
      t.text         :name,       null: false
      t.references   :network,    null: false
      # Both of these should also be CIDRs.
      t.text         :first,      null: false
      t.text         :last,       null: false
      t.timestamps     
      t.text         :conduit,    null: true # 20140619173000
      t.integer      :vlan,       null: true
      t.boolean      :use_vlan,   null: true
      t.boolean      :use_bridge, null: true
      t.integer      :team_mode,  null: true
      t.boolean      :use_team,   null: true
      t.boolean      :overlap,    null: false, default: false   # 20150325101300
    end
    add_index "network_ranges", [:network_id, :name], unique: true

    create_table "network_allocations" do |t|
      t.references   :node
      t.references   :network,    null: false
      t.references   :network_range, null: false
      t.text         :address,    null: false, index: { unique: true }
      t.timestamps
    end

    # 20150613153000
    create_table "dns_name_filters" do |t|
      t.text        :name,        null: false, default: 'default'
      t.text        :matcher,     null: false, default: 'net.category == "admin"'
      t.integer     :priority,    null: false, default: 50, index: { unique: true }
      t.text        :service,     null: false, default: 'system'
      t.text        :template,    null: false, default: '{{node.name}}'
      t.timestamps
    end

    create_table "dns_name_entries" do |t|
      t.belongs_to  :network_allocation,    null: false
      t.belongs_to  :dns_name_filter,       null: false
      t.text        :name,                  null: false, default: ''
      t.text        :rr_type
    end

    # 2014070915300
    create_table :available_hammers do |t|
      t.text       :name,               null: false, index: :unique
      t.text       :klass,              null: false, index: :unique
      t.integer    :priority,           null: false
      t.timestamps
    end

    # 2014070915300
    create_table :hammers do |t|
      t.belongs_to :node,               null: false
      t.belongs_to :available_hammer,   null: false
      t.integer    :priority,           null: false, default: 0
      t.text       :type,               null: false, foreign_key: { references: [:available_hammers, :klass] }
      t.text       :name,               null: false, foreign_key: { references: [:available_hammers, :name] }
      # How the node manager should talk to the node being managed.
      # How this is parsed is hammer dependent.
      t.text       :endpoint,           null: true
      # The username that the manager should use for this node.
      t.text       :username,           null: false, default: ''
      # The authenticating token that the manager should use for this node.
      # It could be a password, it could be half of a keypair, or it
      # could be something entirely different.
      t.text       :authenticator,      null: true
    end
    # Only one manager of a given type can be bound to a node.
    add_index(:hammers, [:node_id, :type], unique: true)

    create_table :runs do |t|
      t.belongs_to  :node_role,        null: false, index: true
      t.belongs_to  :node,             null: false, index: true
      t.text        :run_data,         null: false
      t.boolean     :running,          null: false, default: false, index: true
      t.text        :queue,            null: false, default: "NodeRoleRunner"   # 20150504102000
      t.integer     :job_id,           references: nil  # 20150513214112
    end

    # 20150509000057
    create_table :audits, :force => true do |t|
      t.column :auditable_id,     :integer, references: nil
      t.column :auditable_type,   :text
      t.column :associated_id,    :integer, references: nil
      t.column :associated_type,  :text
      t.column :user_id,          :integer, references: nil
      t.column :user_type,        :text
      t.column :username,         :text
      t.column :action,           :text
      t.column :audited_changes,  :text
      t.column :version,          :integer, :default => 0
      t.column :comment,          :text
      t.column :remote_address,   :text
      t.column :request_uuid,     :text, index: true
      t.column :created_at,       :datetime, index: true
    end
    add_index :audits, [:auditable_id, :auditable_type], :name => 'auditable_index'
    add_index :audits, [:associated_id, :associated_type], :name => 'associated_index'
    add_index :audits, [:user_id, :user_type], :name => 'user_index'

    # 20150619171000
    create_table "audit_trackers" do |t|
      t.text         :tracker,       index: { unique: true }
      t.integer      :processed,     default: 0
    end

    # PERFORMANCES VIEWS


    create_view :provisioner_database, "select n.name as name,
                                        n.bootenv as bootenv,
                                        a.address as address,
                                        jsonb_extract_path(n.discovery,'ohai','network','interfaces') as discovered_macs,
                                        jsonb_extract_path(n.hint,'admin_macs') as hinted_macs
                                 from nodes n
                                 inner join network_allocations a on a.node_id = n.id
                                                                  and family(a.address::inet) = 4
                                 inner join networks net on a.network_id = net.id and net.category = 'admin'
                                 inner join node_roles nr on nr.node_id = n.id
                                 inner join roles r on nr.role_id = r.id and r.name = 'rebar-managed-node'
                                 where jsonb_extract_path(n.hint,'admin_macs') is not null
                                 or jsonb_extract_path(n.discovery,'ohai','network','interfaces') is not null"

    create_view :dhcp_database, "select n.name as name,
                                        n.bootenv as bootenv,
                                        a.address as address,
                                        jsonb_extract_path(n.discovery,'ohai','network','interfaces') as discovered_macs,
                                        jsonb_extract_path(n.hint,'admin_macs') as hinted_macs
                                 from nodes n
                                 inner join network_allocations a on a.node_id = n.id
                                                                  and family(a.address::inet) = 4
                                 inner join networks net on a.network_id = net.id and net.category = 'admin'
                                 inner join node_roles nr on nr.node_id = n.id
                                 inner join roles r on nr.role_id = r.id and r.name = 'rebar-managed-node'
                                 where jsonb_extract_path(n.hint,'admin_macs') is not null
                                 or jsonb_extract_path(n.discovery,'ohai','network','interfaces') is not null"

    create_view :docker_database, "select n.name as name,
                                       a.address as address
                                from nodes n
                                inner join network_allocations a on n.id = a.node_id
                                inner join networks net on net.id = a.network_id
                                inner join node_roles nr on nr.node_id = n.id
                                inner join roles r on nr.role_id = r.id
                                where net.category = 'admin'
                                and r.name = 'rebar-docker-node'"

    create_view :dns_database, "select n.name as name,
                                       a.address as address,
                                       net.name as network,
                                       net.category as category
                                from nodes n
                                inner join network_allocations a on n.id = a.node_id
                                inner join networks net on net.id = a.network_id"

    create_view :all_role_requires_paths, "with recursive arrp (child_name, parent_name, path) as (
                                select r.name, rr.requires, ARRAY[r.name::text,rr.requires::text]
                                from role_requires rr, roles r where r.id = rr.role_id
                                union
                                select arrp.child_name, rr.requires, arrp.path || rr.requires::text
                                from arrp, roles r, role_requires rr
                                where r.id = rr.role_id AND r.name = arrp.parent_name)
                                select child_name, parent_name, path from arrp;"

    create_view :all_role_requires, "with recursive arr (role_id, required_role_id, required_role_name) as (
                                select role_id, required_role_id, requires from role_requires
                                union
                                select arr.role_id, rr.required_role_id, rr.requires
                                from arr, role_requires rr
                                where rr.requires  = arr.required_role_name)
                                select role_id, required_role_id, required_role_name from arr;"


  end

end
