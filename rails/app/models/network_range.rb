# Copyright 2013, Dell
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

class NetworkRange < ActiveRecord::Base

  audited

  after_commit :on_change_hooks

  validate :sanity_check_range

  belongs_to  :network
  has_many    :network_allocations,   :dependent => :destroy
  has_many    :nodes,                 :through=>:network_allocations

  alias_attribute :allocations,       :network_allocations

  def as_json(*args)
    super(*args).merge({"first" => first.to_s, "last" =>  last.to_s})
  end

  def first
    IP.coerce(read_attribute("first"))
  end

  def first=(addr)
    write_attribute("first",IP.coerce(addr).to_s)
  end

  def fullname
    "#{network.name}.#{name}"
  end

  def last
    IP.coerce(read_attribute("last"))
  end

  def last=(addr)
    write_attribute("last",IP.coerce(addr).to_s)
  end

  def conduit
    read_attribute("conduit") || network.conduit
  end

  def vlan
    read_attribute("vlan") || network.vlan
  end

  def team_mode
    read_attribute("team_mode") || network.team_mode
  end

  def use_vlan
    res = read_attribute("use_vlan")
    res = network.use_vlan if res.nil?
    res
  end

  def use_bridge
    res = read_attribute("use_bridge")
    res = network.use_bridge if res.nil?
    res
  end

  def use_team
    res = read_attribute("use_team")
    res = network.use_team if res.nil?
    res
  end

  def === (other)
    addr = IP.coerce(other)
    (first <= addr) && (addr <= last)
  end

  def allocate(node, suggestion = nil)
    res = NetworkAllocation.find_by(:node_id => node.id, :network_range_id => self.id)
    return res if res
    unless suggestion
      attr_name = "hint-#{network.name}-"
      if first.v4?
        attr_name << "v4addr"
      else
        attr_name << "v6addr"
      end
      suggestion = Attrib.get(attr_name,node)
    end
    suggestion ||= node.auto_v6_address(network) if first.v6?
    begin
      Rails.logger.info("NetworkRange: allocating address from #{fullname} for #{node.name} with suggestion #{suggestion}")
      NetworkAllocation.locked_transaction do
        # Use suggestion if it fits
        if suggestion
          suggestion = IP.coerce(suggestion)
          if (self === suggestion) &&
              (NetworkAllocation.where(:address => suggestion.to_s).count == 0)
            res = NetworkAllocation.create!(:network_range_id => self.id,
                                            :network_id => network_id,
                                            :node_id => node.id,
                                            :address => suggestion)
          end
        end
        # Use octet aligned suggestion if admin network exists
        unless res
          my_na = nil
          NetworkAllocation.node_cat(node, "admin").each do |na|
            next unless na.address.class == self.first.class
            next unless na.network.group == self.network.group
            my_na = na
            break
          end

          # If we have an admin network for this group, use its host part as a suggestion
          if my_na
            suggestion = self.first.with_host(my_na.address.host)
            if (self === suggestion) &&
                (NetworkAllocation.where(:address => suggestion.to_s).count == 0)
              res = NetworkAllocation.create!(:network_range_id => self.id,
                                              :network_id => network_id,
                                              :node_id => node.id,
                                              :address => suggestion)
            end
          end
        end

        # Still no address, try and find an unused one.
        unless res
          addr = nil
          allocated = network_allocations.all.map{|a|a.address}.sort{|a,b| b <=> a}
          if allocated.empty?
            addr = first
          else
            (first..last).each do |a|
              next if a == allocated.pop
              addr = a
              break
            end
            raise RangeError.new("#{fullname} is out of addresses!") unless addr
          end
          res = NetworkAllocation.create!(:network_range_id => self.id,
                                          :network_id => network_id,
                                          :node_id => node.id,
                                          :address => addr.to_s)
        end
      end
    end
    Rails.logger.info("NetworkRange: #{node.name} allocated #{res.address} from #{fullname}")
    network.make_node_role(node)
    res
  end

  private

  def sanity_check_range
    unless network
      errors.add(:network, "NetworkRange does not have an associated network!")
    else
      # Check conduit, vlan, team, and bond sanity
      Network.check_sanity(self).each do |err|
        errors.add(err[0], "NetworkRange #{fullname}: #{err[1]}")
      end
    end

    errors.add(:network, "NetworkRange #{fullname}: must have a non-configure parent network with specifying overlap") if (overlap and network and network.configure)

    unless first.subnet == last.subnet
      errors.add(:first, "NetworkRange #{fullname}: #{first.to_s} and #{last.to_s} must be of the same netmask")
    end
    unless first.class == last.class
      errors.add(:first, "NetworkRange #{fullname}: #{first.to_s} and #{last.to_s} must be of the same type")
    end
    unless first.network == last.network
      errors.add(:first, "NetworkRange #{fullname}: #{first.to_s} and #{last.to_s} must be in the same subnet")
    end
    if first.network == first
      errors.add(:first, "NetworkRange #{fullname}: #{first} cannot be a subnet address")
    end
    if last.broadcast == last
      errors.add(:last, "NetworkRange #{fullname}: #{last} cannot be a broadcast address")
    end
    if first.broadcast == first
      errors.add(:first, "NetworkRange #{fullname}: #{first} cannot be a broadcast address")
    end
    if last.network == last
      errors.add(:last, "NetworkRange #{fullname}: #{last} cannot be a subnet address")
    end

    # Now, verify that this range does not overlap with any other range

    NetworkRange.transaction do
      NetworkRange.all.each do |other|
        next if other.id == id

        if !other.overlap and !overlap && (other === first or self === other.first)
          errors.add(:first, "NetworkRange #{fullname}: first address #{first.to_s} overlaps with range #{other.fullname}")
        end
        if !other.overlap and !overlap and (other === last or self === other.last)
          errors.add(:last, "NetworkRange #{fullname}: last address #{last.to_s} overlaps with range #{other.fullname}")
        end
        if network && !Network.check_conduit_sanity(conduit, other.conduit)
          errors.add(:conduit, "NetworkRange #{fullname}: Conduit mapping overlaps with network range #{other.fullname}")
        end
      end
    end
  end

  # Call the on_network_change hooks.
  def on_change_hooks
    # do the low cohorts last
    Rails.logger.info("NetworkRange: calling all role on_network_change hooks for #{network.name}")
    Role.all_cohorts_desc.each do |r|
      begin
        Rails.logger.info("NetworkRange: Calling #{r.name} on_network_change for #{self.network.name}")
        r.on_network_change(self.network)
      rescue Exception => e
        Rails.logger.error "NetworkRange #{self.name} attempting to change role #{r.name} failed with #{e.message}"
      end
    end
  end


end
