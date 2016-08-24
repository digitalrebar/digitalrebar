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

require 'rest-client'
require 'uri'

class BarclampDns::MgmtService < Service

  def do_transition(nr,data)
    deployment_role = nr.deployment_role
    until Attrib.get('dns-management-servers',deployment_role) do
      sleep 1
      deployment_role.reload
    end
  end

  def on_active(nr)
    # Preset all the pre-existing allocations.
    NetworkAllocation.all.each do |na|
      DnsNameFilter.claim_by_any(na)
    end
  end

  def on_node_change(n)
    NetworkAllocation.node(n).each do |na|
      DnsNameFilter.claim_by_any(na)
    end
  end

  def on_network_allocation_create(na)
    DnsNameFilter.claim_by_any(na)
  end

  def on_network_allocation_delete(na)
    DnsNameEntry.for_network_allocation(na).each do |dne|
      dne.destroy!
    end
  end

  def self.remove_ip_address(dne)
    self.update_ip_address(dne, 'REMOVE')
  end

  def self.add_ip_address(dne)
    update_ip_address(dne, 'ADD')
  end

  def self.update_ip_address(dne, action)
    address = dne.network_allocation.address
    name, domain = dne.name.split('.', 2)
    self.update_dns_record(domain, dne.tenant_id, dne.rr_type, name, address.addr, action)
  end

  def self.send_request(url,data)
    TrustedClient.new(url).patch(data.to_json)
  end

  def self.update_dns_record(zone, t_id, rr_type, name, value, action)
    url = TrustedService.url("dns-mgmt-service")
    return if url.nil?

    data = {
        'tenant_id' => t_id,
        'changetype' => action,
        'name' => name,
        'content' => value,
        'type' => rr_type
    }

    send_request("#{url}/zones/#{zone}", data)
  end

end
