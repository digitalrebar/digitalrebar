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

class BarclampDns::MgmtService < Service

  def template
    # this is a workable solution for now, we use the admin node to determine domain (except when non-exists!)
    domain = Node.admin.first.name.split('.',2)[1] rescue I18n.t('not_set')
    {'crowbar' => {     'dns' => {
        'domain' => domain,
        'contact' => 'support@localhost.localdomain',
        'forwarders' =>  [],
        'static' => {},
        'ttl' => '1h',
        'slave_refresh' => '1d',
        'slave_retry' => '2h',
        'slave_expire' => '4w',
        'negative_cache' => 300}}}
  end


  def do_transition(nr,data)
    internal_do_transition(nr, data, 'dns-mgmt-service', 'dns-management-servers') do |s|
      Rails.logger.debug("DnsMgmtServer: #{s.inspect} #{s.ServiceAddress}")
      addr = IP.coerce(s.ServiceAddress)
      Rails.logger.debug("DnsMgmtServer: #{addr.inspect}")

      server_name = s.ServiceTags.first
      cert_pem = ConsulAccess.getKey("opencrowbar/private/dns-mgmt/#{server_name}/cert_pem")
      access_name = ConsulAccess.getKey("opencrowbar/private/dns-mgmt/#{server_name}/access_name")
      access_password = ConsulAccess.getKey("opencrowbar/private/dns-mgmt/#{server_name}/access_password")

      url = "https://#{access_name}:#{access_password}@"
      if addr.v6?
        url << "[#{addr.addr}]"
      else
        url << addr.addr
      end
      url << ":#{s.ServicePort}"

      { 'address' => s.ServiceAddress,
        'port' => "#{s.ServicePort}",
        'name' => server_name,
        'cert' => cert_pem,
        'access_name' => access_name,
        'access_password' => access_password,
        'url' => url}
    end
  end

  def on_active(nr)
    # Preset all the pre-existing allocations.

    services = Attrib.get('dns-management-servers', nr)
    slist = []
    shash = {}
    addrs = {}
    services.each do |s|
      svc = s['name']
      slist << svc
      shash[svc] = s
      addrs[svc] = []
    end

    Rails.logger.fatal("GREG: Start claiming")

    NetworkAllocation.all.each do |na|
      DnsNameFilter.claim_by_any(na)
    end

    Rails.logger.fatal("GREG: Done claiming")
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
    dne = DnsNameEntry.for_network_allocation(na).first
    dne.release if dne
  end

  def self.get_service(service_name)
    service = nil
    # This is not cool, but should be small in most environments.
    BarclampDns::MgmtService.all.each do |role|
      role.node_roles.each do |nr|
        services = Attrib.get('dns-management-servers', nr)
        next unless services
        services.each do |s|
          service = s if s['name'] == service_name
          return service if service
        end
      end
    end
    nil
  end

  def self.remove_ip_address(dne)
    service = get_service(dne.dns_name_filter.service)
    return unless service

    address = dne.network_allocation.address
    name, domain = dne.name.split('.')
    self.remove_dns_record(service, domain, dne.rr_type, name, address.addr, true)
  end

  def self.add_ip_address(dne)
    service = get_service(dne.dns_name_filter.service)
    return unless service

    address = dne.network_allocation.address
    name, domain = dne.name.split('.')
    self.replace_dns_record(service, domain, dne.rr_type, name, address.addr, true)
  end

  def self.send_request(url, data, ca_string)
    store = OpenSSL::X509::Store.new
    store.add_cert(OpenSSL::X509::Certificate.new(ca_string))
    
    RestClient::Resource.new(
        url,
        :ssl_cert_store =>  store,
        :verify_ssl     =>  OpenSSL::SSL::VERIFY_PEER
    ).patch data.to_json, :content_type => :json, :accept => :json
  end

  def self.replace_dns_record(service, zone, rr_type, name, value, setptr)
    Rails.logger.fatal("GREG: replace_dns_record: #{service['name']} #{zone} #{rr_type} #{name} #{value} #{setptr}")

    url = "#{service['url']}/zones/#{zone}"

    data = {
        'rrsets' => [
            {
                'name' => name,
                'type' => rr_type,
                'changetype' => 'REPLACE',
                'records' => [
                    {
                        'content' => value,
                        'disabled' => false,
                        'name' => name,
                        'ttl' => 3600,
                        'type' => rr_type,
                        'setptr' => setptr,
                        'priority' => 0
                    }
                ]
            }
        ]
    }

    Rails.logger.fatal("GREG: replace dns record: #{url}")

    send_request(url, data, service['cert'])
  end

  def self.remove_dns_record(service, zone, rr_type, name, value, setptr)
    Rails.logger.fatal("GREG: remove_dns_record: #{service['name']} #{zone} #{rr_type} #{name} #{setptr}")

    url = "#{service['url']}/zones/#{zone}"
    data = {
        'rrsets' => [
            {
                'name' => name,
                'type' => rr_type,
                'changetype' => 'DELETE',
                'records' => [ ]
            }
        ]
    }

    send_request(url, data, service['cert'])
  end

end
