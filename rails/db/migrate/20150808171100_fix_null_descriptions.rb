
class FixNullDescriptions < ActiveRecord::Migration

  def up
    # Change all character varying columns to text
    # Text is more efficinet in postgresql.
    drop_view :all_role_requires
    drop_view :all_role_requires_paths
    drop_view :provisioner_database
    drop_view :dhcp_database
    drop_view :docker_database
    drop_view :dns_database

    change_column :role_requires, :requires, :text
    change_column :roles, :name, :text
    change_column :roles, :description, :text, default: '', null: false
    change_column :roles, :"type", :text
    change_column :roles, :jig_name, :text
    change_column_null :roles, :milestone, false
    change_column_null :roles, :powersave, false
    change_column :attribs, :type, :text
    change_column :attribs, :name, :text
    change_column :attribs, :description, :text, default: '', null: false
    change_column :attribs, :map, :text, null: false, default: ''
    change_column :attribs, :ui_renderer, :text, default: 'attribs/default'
    change_column :audit_trackers, :tracker, :text
    change_column :available_hammers, :name, :text
    change_column :available_hammers, :klass, :text
    change_column :barclamps, :name, :text, null: false, default: ''
    change_column :barclamps, :description, :text, default: '', null: false
    change_column :barclamps, :source_url, :text
    change_column :barclamps, :source_path, :text
    change_column :barclamps, :commit, :text, default: 'unknown'
    change_column :barclamps, :build_version, :text, default: 'unknown'
    change_column :deployments, :name, :text
    change_column :deployments, :description, :text, default: '', null: false
    change_column :network_allocations, :address, :text
    change_column_null :network_allocations, :network_id, false
    change_column_null :network_allocations, :network_range_id, false
    change_column :networks, :name, :text
    change_column :networks, :description, :text, default: '', null: false
    change_column :networks, :v6prefix, :text
    change_column :networks, :conduit, :text
    change_column :networks, :category, :text, default: 'general'
    change_column :networks, :"group", :text, default: 'default'
    change_column :networks, :pbr, :text
    change_column_null :networks, :deployment_id, false
    change_column :node_roles, :status, :text, null: false, default: ''
    change_column_null :node_roles, :order, false
    change_column :nodes, :name, :text
    change_column :nodes, :description, :text, default: '', null: false
    change_column :nodes, :name, :text
    change_column :nodes, :bootenv, :text, default: 'sledgehammer'
    change_column_null :nodes, :order, false
    change_column :dns_name_entries, :name, :text, null: false, default: ''
    change_column :dns_name_entries, :rr_type, :text
    change_column_null :dns_name_entries, :network_allocation_id, false
    change_column_null :dns_name_entries, :dns_name_filter_id, false
    change_column :dns_name_filters, :name, :text, default: 'default', null: false
    change_column :dns_name_filters, :matcher, :text, default: 'net.category == "admin"', null: false
    change_column :dns_name_filters, :service, :text, default: 'system', null: false
    change_column :dns_name_filters, :template, :text, default: '{{node.name}}', null: false
    change_column_null :dns_name_filters, :priority, false
    change_column :groups, :name, :text
    change_column :groups, :description, :text, default: '', null: false
    change_column :groups, :category, :text, default: 'ui'
    change_column :hammers, :type, :text
    change_column :hammers, :name, :text
    change_column :hammers, :username, :text
    change_column :jigs, :name, :text, null: false, unique: true
    change_column :jigs, :description, :text, default: '', null: false
    change_column :jigs, :type, :text
    change_column :jigs, :client_role_name, :text
    change_column :jigs, :server, :text
    change_column :jigs, :client_name, :text
    change_column_null :jigs, :order, false
    change_column_null :jigs, :active, false
    change_column :network_ranges, :name, :text
    change_column :network_ranges, :first, :text
    change_column :network_ranges, :last, :text
    change_column :network_ranges, :conduit, :text
    change_column_null :network_ranges, :network_id, false
    change_column :network_routers, :address, :text
    change_column_null :network_routers, :network_id, false
    change_column :role_require_attribs, :attrib_name, :text
    change_column :role_require_attribs, :attrib_at, :text
    change_column :runs, :queue, :text, default: 'NodeRoleRunner'


    create_view :provisioner_database, "select n.name as name,
                                        n.bootenv as bootenv,
                                        a.address as address,
                                        json_extract_path(n.discovery,'ohai','network','interfaces') as discovered_macs,
                                        json_extract_path(n.hint,'admin_macs') as hinted_macs
                                 from nodes n
                                 inner join network_allocations a on a.node_id = n.id
                                                                  and family(a.address::inet) = 4
                                 inner join networks net on a.network_id = net.id and net.category = 'admin'
                                 inner join node_roles nr on nr.node_id = n.id
                                 inner join roles r on nr.role_id = r.id and r.name = 'crowbar-managed-node'
                                 where json_extract_path(n.hint,'admin_macs') is not null
                                 or json_extract_path(n.discovery,'ohai','network','interfaces') is not null"

    create_view :dhcp_database, "select n.name as name,
                                        n.bootenv as bootenv,
                                        a.address as address,
                                        json_extract_path(n.discovery,'ohai','network','interfaces') as discovered_macs,
                                        json_extract_path(n.hint,'admin_macs') as hinted_macs
                                 from nodes n
                                 inner join network_allocations a on a.node_id = n.id
                                                                  and family(a.address::inet) = 4
                                 inner join networks net on a.network_id = net.id and net.category = 'admin'
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
                                where net.category = 'admin'
                                and r.name = 'crowbar-docker-node'"

    create_view :dns_database, "select n.name as name,
                                       a.address as address,
                                       net.name as network,
                                       net.category as category
                                from nodes n
                                inner join network_allocations a on n.id = a.node_id
                                inner join networks net on net.id = a.network_id"
    create_view :all_role_requires_paths, "
with recursive arrp (child_name, parent_name, path) as (
    select r.name, rr.requires, ARRAY[r.name::text,rr.requires::text]
    from role_requires rr, roles r where r.id = rr.role_id
    union
    select arrp.child_name, rr.requires, arrp.path || rr.requires::text
    from arrp, roles r, role_requires rr
    where r.id = rr.role_id AND r.name = arrp.parent_name)
    select child_name, parent_name, path from arrp;"

    create_view :all_role_requires, "
with recursive arr (role_id, required_role_id, required_role_name) as (
    select role_id, required_role_id, requires from role_requires
    union
    select arr.role_id, rr.required_role_id, rr.requires
    from arr, role_requires rr
    where rr.requires  = arr.required_role_name)
    select role_id, required_role_id, required_role_name from arr;"



  end
end
