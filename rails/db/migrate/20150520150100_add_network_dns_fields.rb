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

class AddNetworkDnsFields < ActiveRecord::Migration

  def change
    change_table :networks do |t|
      t.boolean   :update_dns,   null: false, default: false
      t.string    :dns_domain,   null: true
      t.string    :hostname_template, null: false, default: "{{node.name}}"
      t.string    :dns_svc_name, null: true
    end

    change_table :network_ranges do |t|
      t.boolean   :update_dns,   null: true
      t.string    :dns_domain,   null: true
      t.string    :hostname_template, null: true
      t.string    :dns_svc_name, null: true
    end
  end
end