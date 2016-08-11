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
    Rails.logger.debug('dhcp-mgmt on_active start - update subnet')
    Network.in_category('admin').each do |net|
      on_network_create(net)
    end

    Rails.logger.debug('dhcp-mgmt on_active start - update individual nodes')
    NetworkAllocation.all.each do |na|
      on_network_allocation_create(na)
    end
    Rails.logger.debug('dhcp-mgmt on_active start - done')
  end

  def build_network(network)
    # For now, only do admin networks
    return unless network.category == 'admin'

    r = network.ranges.find_by(name: "dhcp")
    r = network.ranges.find_by(name: "host") unless r
    unless r
      # All goes away, makes sure we pull it.
      begin
        self.class.delete_network(network.name)
      rescue RestClient::ResourceNotFound
        return
      end
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
    Rails.logger.debug('Missing next_server') unless next_server

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
    Rails.logger.debug('Missing dns_server') unless dns_server.empty?
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
    Rails.logger.debug('Missing ntp_server') unless ntp_server.empty?
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
      self.class.create_network(network.tenant_id, network.name, subnet, next_server, start_ip, end_ip, options)
    rescue
      begin
        self.class.update_network(network.tenant_id, network.name, subnet, next_server, start_ip, end_ip, options)
      rescue Exception => e
        Rails.logger.fatal("Failed to update: #{network.name}: #{e.message}")
        raise e
      end
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
    Rails.logger.info("dhcp-mgmt on_network_allocation_create - #{na.address} net cat #{na.network.category}")
    return unless na.network.category == 'admin'

    Rails.logger.info("dhcp-mgmt on_network_allocation_create - v4? #{na.address.v4?}")
    return unless na.address.v4?
    loader = Attrib.get('provisioner-bootloader',na.node)

    # Get the mac addresses
    ints = na.node.discovery['ohai']['network']['interfaces'] rescue nil
    mac_list = na.node.hint['admin_macs'] || []
    Rails.logger.info("dhcp-mgmt on_network_allocation_create - ints = #{ints}  mac_list = #{mac_list}")
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

    Rails.logger.info("dhcp-mgmt on_network_allocation_create - mac_list #{mac_list}")
    mac_list.each do |mac|
      begin
        self.class.bind_node_ip_mac(na.network.name, mac, na.address.addr, na.node.bootenv, loader)
      rescue Exception => e
        Rails.logger.warn("Tring to remove DHCP binding for #{mac}: #{e.message}")
      end
    end
    Rails.logger.info("dhcp-mgmt on_network_allocation_create - done #{na.address}")
  end

  def on_network_allocation_delete(na)
    Rails.logger.info("dhcp-mgmt on_network_allocation_delete - #{na.address} net cat #{na.network.category}")
    return unless na.network.category == 'admin'
    return unless na.address.v4?

    # Get the mac addresses
    ints = na.node.discovery['ohai']['network']['interfaces'] rescue nil
    mac_list = na.node.hint['admin_macs'] || []
    Rails.logger.info("dhcp-mgmt on_network_allocation_delete - ints = #{ints}  mac_list = #{mac_list}")
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

    Rails.logger.info("dhcp-mgmt on_network_allocation_delete - mac_list = #{mac_list}")
    mac_list.each do |mac|
      begin
        self.class.unbind_node_ip_mac(na.network.name, mac)
      rescue Exception => e
        Rails.logger.warn("Tring to remove DHCP binding for #{mac}: #{e.message}")
      end
    end
    Rails.logger.info("dhcp-mgmt on_network_allocation_delete - done")
  end

  def on_node_change(node)
    return unless node.previous_changes['bootenv']
    # Pay attention to bootenv change
    node.network_allocations.each do |na|
      on_network_allocation_create(na)
    end
  end

  #
  # name is name of network
  # subnet is a CIDR string
  # next_server, start_ip, end_ip is an IP string
  # options is a hash of number => value string
  #
  def self.create_network(t_id, name, subnet, next_server, start_ip, end_ip, options)
    service = get_service
    return unless service

    hash = {
      "name" => name,
      "tenant_id" => t_id,
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

    TrustedClient.new(url).post(hash.to_json)
  end

  def self.update_network(t_id, name, subnet, next_server, start_ip, end_ip, options)
    service = get_service
    return unless service

    hash = {
      "name" => name,
      "subnet" => subnet,
      "tenant_id" => t_id,
      "next_server" => next_server,
      "active_start" => start_ip,
      "active_end" => end_ip
    }
    options.each do |k,v|
      hash["options"] ||= []
      hash["options"] << { "id" => k, "value" => v }
    end

    url = "#{service['url']}/subnets/#{name}"

    TrustedClient.new(url).put(hash.to_json)
  end

  def self.delete_network(name)
    service = get_service
    return unless service
    url = "#{service['url']}/subnets/#{name}"
    TrustedClient.new(url).delete
  end

  def self.bind_node_ip_mac(name, mac, ip, bootenv, loader)

    Rails.logger.info("dhcp-mgmt bind_node_ip_mac - #{name} #{mac} #{ip} #{bootenv} #{loader}")

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
    TrustedClient.new(url).post(hash.to_json)
  end

  def self.unbind_node_ip_mac(name, mac)

    Rails.logger.info("dhcp-mgmt unbind_node_ip_mac - #{name} #{mac}")

    service = get_service
    return unless service
    url = "#{service['url']}/subnets/#{name}/bind/#{mac}"
    TrustedClient.new(url).delete
  end

end
