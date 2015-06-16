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
class DnsNameFilter < ActiveRecord::Migration

  def self.up
    create_table "dns_name_filters" do |t|
      t.string      :name,        default: 'default'
      t.string      :matcher,     default: 'net.category == "admin"'
      t.integer     :priority,    default: 50, index: { unique: true }
      t.string      :service,     default: 'system'
      t.string      :template,    default: '{{node.name}}'
      t.timestamps
    end

    create_table "dns_name_entries" do |t|
      t.belongs_to  :network_allocation
      t.belongs_to  :dns_name_filter
      t.string      :name
      t.string      :rr_type
    end
  end

  def self.down
    drop_table "dns_name_filters"
    drop_table "dns_name_entries"
  end

end

