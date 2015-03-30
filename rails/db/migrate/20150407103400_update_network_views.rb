# Copyright 2015, Greg Althaus
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
class UpdateNetworkViews < ActiveRecord::Migration

  def self.up
    drop_view :provisioner_database
    drop_view :dhcp_database
    drop_view :docker_database
    drop_view :dns_database

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
                                       n.alias as cname,
                                       a.address as address,
                                       net.name as network,
                                       net.category as category
                                from nodes n
                                inner join network_allocations a on n.id = a.node_id
                                inner join networks net on net.id = a.network_id"

  end

  def self.down
    drop_view :provisioner_database
    drop_view :dhcp_database
    drop_view :docker_database
    drop_view :dns_database

    create_view :provisioner_database, "select n.name as name,
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

    create_view :dns_database, "select n.name as name,
                                       n.alias as cname,
                                       a.address as address,
                                       net.name as network
                                from nodes n
                                inner join network_allocations a on n.id = a.node_id
                                inner join networks net on net.id = a.network_id"
  end

end
