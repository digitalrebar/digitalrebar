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

class RemoveDhcpDatabaseView < ActiveRecord::Migration

  def self.up
    drop_view :dhcp_database
  end

  def self.down
    create_view :dhcp_database, "select n.name as name,
                                        n.bootenv as bootenv,
                                        a.address as address,
                                        jsonb_extract_path(n.discovery,'ohai','network','interfaces') as discovered_macs,
                                        jsonb_extract_path(n.hint,'admin_macs') as hinted_macs
                                 from nodes n
                                 inner join network_allocations a on a.node_id = n.id
                                                                  and family(a.address::inet) = 4
                                 inner join networks net on a.network_id = net.id and net.category = 'admin'
                                 where jsonb_extract_path(n.hint,'admin_macs') is not null
                                 or jsonb_extract_path(n.discovery,'ohai','network','interfaces') is not null"
  end
end
