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

require 'resolv'
require 'rest-client'
require 'uri'

class BarclampDhcp::MgmtService < Service

  def self.bootloader(loader)
    # If the system is already using iPXE (e.g. KVM and friends), continue to use it.
    case loader
    when 'ipxe'
      '{{if (eq (index . 77) "iPXE") }}default.ipxe{{else if (eq (index . 93) "0")}}ipxe.pxe{{else}}ipxe.efi{{end}}'
    when 'lpxelinux'
      '{{if (eq (index . 77) "iPXE") }}default.ipxe{{else if (eq (index . 93) "0")}}lpxelinux.0{{else}}bootx64.efi{{end}}'
    when 'lpxelinux-only'
      '{{if (eq (index . 93) "0")}}lpxelinux.0{{else}}bootx64.efi{{end}}'
    else
      Rails.logger.fatal("Unknown boot loader #{loader}")
    end
  end

  def do_transition(nr,data)
    wait_for_service(nr, data, 'dhcp-mgmt-service')
    deployment_role = nr.deployment_role
    until Attrib.get('dhcp-management-servers',deployment_role) &&
          Attrib.get('provisioner-webservers',deployment_role.deployment)
      sleep 1
      deployment_role.reload
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

  def build_network(network)
    # For now, only do admin networks
    return if network.category != 'admin'

    r = network.ranges.find_by(name: "dhcp")
    r = network.ranges.find_by(name: "host") unless r
    unless r
      # All goes away, makes sure we pull it.
      self.class.delete_network(network.name)
      return
    end

    start_ip = r.first.addr
    end_ip = r.last.addr
    subnet = r.first.network.to_s

    next_server = nil
    boot_program = 'lpxelinux'
    BarclampProvisioner::Service.all.each do |role|
      role.node_roles.each do |nr|
        services = Attrib.get('provisioner-webservers', nr)
        next unless services

        boot_program = Attrib.get('provisioner-default-boot-program', nr)
        next_server = services[0]['address']
        break
      end
      break if next_server
    end
    Rails.logger.fatal('Missing next_server') unless next_server

    options = {}

    # Option 6 - name servers
    # Option 15 - domain name
    dns_server = []
    dns_domain = nil
    BarclampDns::Service.all.each do |role|
      role.node_roles.each do |nr|
        services = Attrib.get('dns_servers', nr)
        next unless services

        dns_server = services.map { |s| s['address'] }
        dns_domain = Attrib.get('dns-domain', nr)
        break
      end
      break unless dns_server.empty?
    end
    Rails.logger.fatal('Missing dns_server') unless dns_server.empty?
    unless dns_server.empty?
      options[6] = dns_server.join(',')
      options[15] = dns_domain
    end

    # Option 42 - ntp
    ntp_server = []
    BarclampNtp::Service.all.each do |role|
      role.node_roles.each do |nr|
        ntp_server = Attrib.get('ntp_servers', nr) || []
        break unless ntp_server.empty?
      end
      break unless ntp_server.empty?
    end
    Rails.logger.fatal('Missing ntp_server') unless ntp_server.empty?
    unless ntp_server.empty?
      ips = []
      ntp_server.each do |ns|
        Resolv.each_address(ns) do |ip|
          ips << ip
        end
      end
      options[42] = ips.join(',')
    end

    # Option 3 - gateway
    options[3] = network.network_router.address.addr if network and network.network_router
    options[67] = self.class.bootloader(boot_program)
    begin
      self.class.create_network(network.name, subnet, next_server, start_ip, end_ip, options)
    rescue
      self.class.update_network(network.name, subnet, next_server, start_ip, end_ip, options)
    end
  end

  # Event triggers for node creation.
  # roles should override if they want to handle network addition
  def on_network_create(network)
    build_network(network)
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
    build_network(network)
  end

  def on_network_allocation_create(na)
    return if na.network.category != 'admin'

    return unless na.address.v4?
    loader = Attrib.get('provisioner-bootloader',na.node)

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
        self.class.bind_node_ip_mac(na.network.name, mac, na.address.addr, row['bootenv'], loader)
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

  def self.get_base_resource(url)
    store = OpenSSL::X509::Store.new
    store.add_cert(OpenSSL::X509::Certificate.new(File.read('/var/run/rebar/ca.pem')))

    # get client key and cert
    client_cert = OpenSSL::X509::Certificate.new(File.read('/var/run/rebar/server.crt'))
    client_key  = OpenSSL::PKey::RSA.new(File.read('/var/run/rebar/server.key'), '')

    RestClient::Resource.new(
        url,
        :ssl_cert_store  =>  store,
	:ssl_client_cert =>  client_cert,
	:ssl_client_key  =>  client_key,
        :verify_ssl      =>  OpenSSL::SSL::VERIFY_PEER
    )
  end

  def self.send_request_get(url)
    get_base_resource(url).get
  end

  def self.send_request_put(url, data)
    get_base_resource(url).put data.to_json, :content_type => :json, :accept => :json
  end

  def self.send_request_post(url, data)
    get_base_resource(url).post data.to_json, :content_type => :json, :accept => :json
  end

  def self.send_request_delete(url)
    get_base_resource(url).delete
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

    send_request_post(url, hash)
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

    send_request_put(url, hash)
  end

  def self.delete_network(name)
    service = get_service
    return unless service
    url = "#{service['url']}/subnets/#{name}"
    send_request_delete(url)
  end

  def self.bind_node_ip_mac(name, mac, ip, bootenv, loader)

    hash = {
      "ip" => ip,
      "mac" => mac
    }

    if bootenv == 'local'
      options = {}
    else
      options = {}
      boot_program = loader
      BarclampProvisioner::Service.all.each do |role|
        role.node_roles.each do |nr|
          boot_program = Attrib.get('provisioner-default-boot-program', nr)
          break
        end
      end unless loader
      options[67] = bootloader(boot_program)
    end
    options.each do |k,v|
      hash["options"] ||= []
      hash["options"] << { "id" => k, "value" => v }
    end

    service = get_service
    return unless service
    url = "#{service['url']}/subnets/#{name}/bind"
    send_request_post(url, hash)
  end

  def self.unbind_node_ip_mac(name, mac)
    service = get_service
    return unless service
    url = "#{service['url']}/subnets/#{name}/unbind/#{mac}"
    send_request_post(url, hash)
  end

end
