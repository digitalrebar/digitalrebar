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

class DnsNameFilter < ActiveRecord::Base

  after_create      :load_uuid

  def load_uuid
    self.reload
  end

  private :load_uuid

  has_many :dns_name_entries,   :dependent => :destroy

  # GREG: Add validations for template
  # GREG: add validations for matcher

  after_commit :on_create_hooks, on: :create
  after_commit :on_change_hooks, on: :update

  # Test to see if na should be part of this dnf
  def claims(na)
    tests = matcher.split(',')
    tests.each do |t|
      match = false

      case
        when m = t.match('net.category == "(.*)"')
          match = na.network_range.network.category == m[1]
        when m = t.match('net.name == "(.*)"')
          match = na.network_range.network.name == m[1]
        when m = t.match('range.name == "(.*)"')
          match = na.network_range.name == m[1]
        when m = t.match('deployment.name == "(.*)"')
          match = na.node.deployment.name == m[1]
        when m = t.match('node.role has "(.*)"')
          match = !na.node.node_roles.select { |nr| nr.role.name == m[1] }.empty?
        when m = t.match('node.attr.(.*) == "(.*)"')
          attr = Attrib.get(m[1], na.node) rescue ""
          match = attr == m[2]
        else
          match = false
      end

      return false unless match
    end

    true
  end

  def make_name(na)
    n = na.node
    mac = Attrib.get('hint-admin-macs', n).first rescue ''
    patterns = {
        '{{node.name}}' => (n.name ? n.name.split('.')[0] : ''),
        '{{node.id}}' => n.id.to_s,
        '{{node.mac}}' => (mac ? mac.gsub(':','-') : ''),
        '{{node.deployment}}' => n.deployment.name,
        '{{network.name}}' => na.network.name,
        '{{network.range}}' => na.network_range.name,
        '{{network.category}}' => na.network.category
    }
    lname = template
    patterns.each do |p,v|
      lname = lname.gsub(p,v)
    end
    lname
  end

  def claim_and_update(na)
    # Only if we have an active service should we claim things.
    return false unless BarclampDns::MgmtService.get_service(service)

    # Is there an entry for this pair
    dne = DnsNameEntry.for_network_allocation_and_filter(na, self).first # Should be only

    if claims(na)
      new_name = make_name(na)
      if dne
        if dne.name != new_name
          BarclampDns::MgmtService.remove_ip_address(dne)
          dne.name = new_name
          dne.save!
          BarclampDns::MgmtService.add_ip_address(dne)
        end
      else
        DnsNameEntry.create!(dns_name_filter: self, network_allocation: na, name: new_name, rr_type: (na.address.v4? ? 'A' : 'AAAA'))
      end
      return true
    end

    dne.destroy! if dne
    false
  end

  def self.claim_by_any(na)
    claimed = false
    DnsNameFilter.transaction do
      DnsNameFilter.order("priority ASC").each do |dnf|
        c = dnf.claim_and_update(na)
        claimed ||= c
      end
    end
    claimed
  end

  def on_change_hooks
    NetworkAllocation.all.each do |na|
      DnsNameFilter.claim_by_any(na)
    end
  end

  def on_create_hooks
    return if @after_create
    @after_create = true
    NetworkAllocation.all.each do |na|
      DnsNameFilter.transaction do
        claim_and_update(na)
      end
    end
  end

end
