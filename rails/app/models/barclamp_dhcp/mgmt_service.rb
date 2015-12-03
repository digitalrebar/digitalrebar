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

class BarclampDhcp::MgmtService < Service

  def do_transition(nr,data)
    internal_do_transition(nr, data, 'dhcp-mgmt-service', 'dhcp-management-servers') do |s|
      str_addr = s.ServiceAddress
      str_addr = s.Address if str_addr.nil? or str_addr.empty?
      Rails.logger.debug("DhcpMgmtService: #{s.inspect} #{str_addr}")
      addr = IP.coerce(str_addr)
      Rails.logger.debug("DhcpMgmtService: #{addr.inspect}")

      server_name = s.ServiceTags.first
      cert_pem = ConsulAccess.getKey("digitalrebar/private/dhcp-mgmt/#{server_name}/cert_pem")
      access_name = ConsulAccess.getKey("digitalrebar/private/dhcp-mgmt/#{server_name}/access_name")
      access_password = ConsulAccess.getKey("digitalrebar/private/dhcp-mgmt/#{server_name}/access_password")

      if addr.v6?
        saddr = "[#{addr.addr}]"
      else
        saddr = addr.addr
      end
      url = URI::HTTPS.build(host: saddr, port: s.ServicePort, userinfo: "#{access_name}:#{access_password}")

      { 'address' => str_addr,
        'port' => "#{s.ServicePort}",
        'name' => server_name,
        'cert' => cert_pem,
        'access_name' => access_name,
        'access_password' => access_password,
        'url' => url.to_s }
    end
  end

  def self.get_service(service_name = 'system')
    service = nil
    # This is not cool, but should be small in most environments.
    BarclampDhcp::MgmtService.all.each do |role|
      role.node_roles.each do |nr|
        next unless nr.active?
        services = Attrib.get('dhcp-management-servers', nr)
        next unless services
        services.each do |s|
          service = s if s['name'] == service_name
          return service if service
        end
      end
    end
    nil
  end

  def on_active(nr)
    Network.in_category('admin').each do |net|
      on_network_create(net)
    end

    NetworkAllocation.all do |na|
      on_network_allocation_create(na)
    end
  end

  # Event triggers for node creation.
  # roles should override if they want to handle network addition
  def on_network_create(network)
    # For now, only do admin networks
    return if network.category != 'admin'

    r = network.ranges.find_by(name: "dhcp")
    r = network.ranges.find_by(name: "host") unless r
    return unless r

    start_ip = r.first.addr
    end_ip = r.last.addr
    subnet = r.first.network.to_s

    next_server = nil
    BarclampProvisioner::Service.all.each do |role|
      role.node_roles.each do |nr|
        services = Attrib.get('provisioner-webservers', nr)
        next unless services

        next_server = services[0]['address']
        break
      end
      break if next_server
    end
    Rails.logger.fatal('Missing next_server') unless next_server

    options = {}
    # Option 6 - name servers
    # Option 15 - domain name
    dns_server = nil
    dns_domain = nil
    BarclampDns::Service.all.each do |role|
      role.node_roles.each do |nr|
        services = Attrib.get('dns_servers', nr)
        next unless services

        dns_server = services[0]['address']
        dns_domain = Attrib.get('dns-domain', nr)
        break
      end
      break if dns_server
    end
    Rails.logger.fatal('Missing dns_server') unless dns_server
    options[6] = dns_server
    options[15] = dns_domain

    # Option 3 - gateway
    options[3] = network.network_router.address.addr if network and network.network_router
    # GREG: One day this needs to be selectable by arch
    options[67] = "discovery/lpxelinux.0"

    self.class.create_network(network.name, subnet, next_server, start_ip, end_ip, options)
  end

  # Event triggers for network destruction.
  # roles should override if they want to handle network destruction
  def on_network_delete(network)
    self.class.delete_network(network.name)
  end

  # Event hook that will be called every time a network is saved if any attributes changed.
  # Roles that are interested in watching networks to see what has changed should
  # implement this hook.
  #
  # This does not include IP allocation/deallocation.
  def on_network_change(network)
  end

  def on_network_allocation_create(na)
    return if na.network.category != 'admin'

    return unless na.address.v4?

    # Get the mac addresses
    ActiveRecord::Base.connection.execute("select * from dhcp_database where name='#{na.node.name}'").each do |row|
      ints = JSON.parse(row["discovered_macs"]) if row["discovered_macs"]
      mac_list = row["hinted_macs"] ? JSON.parse(row["hinted_macs"]) : []
      unless ints.nil?
        ints.each do |net, net_data|
          net_data.each do |field, field_data|
            next if field != "addresses"
            field_data.each do |addr, addr_data|
              next if addr_data["family"] != "lladdr"
              mac_list << addr unless mac_list.include? addr
            end
          end
        end
      end

      mac_list.each do |mac| 
        self.class.bind_node_ip_mac(na.network.name, mac, na.address.addr, row['bootenv'])
      end
    end
  end

  def on_network_allocation_delete(na)
    return if na.network.category != 'admin'

    return unless na.address.v4?

    # Get the mac addresses
    ActiveRecord::Base.connection.execute("select * from dhcp_database where name='#{na.node.name}'").each do |row|
      ints = JSON.parse(row["discovered_macs"]) if row["discovered_macs"]
      mac_list = row["hinted_macs"] ? JSON.parse(row["hinted_macs"]) : []
      unless ints.nil?
        ints.each do |net, net_data|
          net_data.each do |field, field_data|
            next if field != "addresses"
            field_data.each do |addr, addr_data|
              next if addr_data["family"] != "lladdr"
              mac_list << addr unless mac_list.include? addr
            end
          end
        end
      end

      mac_list.each do |mac| 
        self.class.unbind_node_ip_mac(na.network.name, mac)
      end
    end
  end

  def on_node_change(node)
    if node.previous_changes[:bootenv] != node.bootenv
      # Pay attention to bootenv change
      node.network_allocations.each do |na|
        on_network_allocation_create(na)
      end
    end
  end

  def self.send_request_put(url, data, ca_string)
    store = OpenSSL::X509::Store.new
    store.add_cert(OpenSSL::X509::Certificate.new(ca_string))

    RestClient::Resource.new(
        url,
        :ssl_cert_store =>  store,
        :verify_ssl     =>  OpenSSL::SSL::VERIFY_PEER
    ).put data.to_json, :content_type => :json, :accept => :json
  end

  def self.send_request_post(url, data, ca_string)
    store = OpenSSL::X509::Store.new
    store.add_cert(OpenSSL::X509::Certificate.new(ca_string))

    RestClient::Resource.new(
        url,
        :ssl_cert_store =>  store,
        :verify_ssl     =>  OpenSSL::SSL::VERIFY_PEER
    ).post data.to_json, :content_type => :json, :accept => :json
  end

  def self.send_request_delete(url, ca_string)
    store = OpenSSL::X509::Store.new
    store.add_cert(OpenSSL::X509::Certificate.new(ca_string))

    RestClient::Resource.new(
        url,
        :ssl_cert_store =>  store,
        :verify_ssl     =>  OpenSSL::SSL::VERIFY_PEER
    ).delete
  end

  #
  # name is name of network
  # subnet is a CIDR string
  # next_server, start_ip, end_ip is an IP string
  # options is a hash of number => value string
  #
  def self.create_network(name, subnet, next_server, start_ip, end_ip, options)
    service = get_service
    return unless service

    hash = {
      "name" => name,
      "subnet" => subnet,
      "next_server" => next_server,
      "active_start" => start_ip,
      "active_end" => end_ip
    }
    options.each do |k,v|
      hash["options"] ||= []
      hash["options"] << { "id" => k, "value" => v }
    end

    url = "#{service['url']}/subnets"

    send_request_post(url, hash, service['cert'])
  end

  def self.update_network(name, subnet, next_server, start_ip, end_ip, options)
    service = get_service
    return unless service

    hash = {
      "name" => name,
      "subnet" => subnet,
      "next_server" => next_server,
      "active_start" => start_ip,
      "active_end" => end_ip
    }
    options.each do |k,v|
      hash["options"] ||= []
      hash["options"] << { "id" => k, "value" => v }
    end

    url = "#{service['url']}/subnets/#{name}"

    send_request_put(url, hash, service['cert'])
  end

  def self.delete_network(name)
    service = get_service
    return unless service
    url = "#{service['url']}/subnets/#{name}"
    send_request_delete(url, service['cert'])
  end

  def self.bind_node_ip_mac(name, mac, ip, bootenv)

    hash = {
      "ip" => ip,
      "mac" => mac
    }

    if bootenv == 'local'
      options = {}
    else
      options = {}
      options[67] = "discovery/lpxelinux.0"
      # GREG: One day this needs to be selectable by arch
    end
    options.each do |k,v|
      hash["options"] ||= []
      hash["options"] << { "id" => k, "value" => v }
    end

    service = get_service
    return unless service
    url = "#{service['url']}/subnets/#{name}/bind"
    send_request_post(url, hash, service['cert'])
  end

  def self.unbind_node_ip_mac(name, mac)
    service = get_service
    return unless service
    url = "#{service['url']}/subnets/#{name}/unbind/#{mac}"
    send_request_post(url, hash, service['cert'])
  end

end

