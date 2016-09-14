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

class DnsNameEntry < ActiveRecord::Base

  after_create      :load_uuid

  def load_uuid
    self.reload
  end

  private :load_uuid

  belongs_to :network_allocation
  belongs_to :dns_name_filter

  before_destroy :on_before_destroy
  after_commit :on_create_hooks, on: :create

  scope :for_filter, ->(dnf) { where(:dns_name_filter_id => dnf.id) }
  scope :for_network_allocation, ->(na) { where(:network_allocation_id => na.id) }
  scope :for_network_allocation_and_filter, ->(na,dnf) { where(:dns_name_filter_id => dnf.id, :network_allocation_id => na.id)}

  def on_before_destroy
    BarclampDns::MgmtService.remove_ip_address(self)
  end

  def on_create_hooks
    return if @after_create
    @after_create = true
    BarclampDns::MgmtService.add_ip_address(self)
  end

end
