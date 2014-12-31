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
  
  validate :sanity_check_range
  
  belongs_to  :network
  has_many    :network_allocations,   :dependent => :destroy
  has_many    :nodes,                 :through=>:network_allocations

  alias_attribute :allocations,       :network_allocations

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
    network.use_vlan if res.nil?
  end

  def use_bridge
    res = read_attribute("use_bridge")
    network.use_bridge if res.nil?
  end

  def use_team
    res = read_attribute("use_team")
    network.use_team if res.nil?
  end

  def === (other)
    addr = IP.coerce(other)
    (first <= addr) && (addr <= last) 
  end

  def allocate(node, suggestion = nil)
    res = NetworkAllocation.where(:node_id => node.id, :network_range_id => self.id).first
    return res if res
    begin
      Rails.logger.info("NetworkRange: allocating address from #{fullname} for #{node.name} with suggestion #{suggestion}")
      NetworkAllocation.locked_transaction do
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
      errors.add("NetworkRange does not have an associated network!")
    end

    # Check conduit, vlan, team, and bond sanity
    Network.check_sanity(self).each do |err|
      errors.add("NetworkRange #{fullname}: #{err}")
    end

    unless name
      errors.add("NetworkRange must have a name")
    end

    unless first.class == last.class
      errors.add("NetworkRange #{fullname}: #{first.inspect} and #{last.inspect} must be of the same type")
    end
    unless first.network == last.network
      errors.add("NetworkRange #{fullname}: #{first.to_s} and #{last.to_s} must be in the same subnet")
    end
    if first.network == first
      errors.add("NetworkRange #{fullname}: #{first} cannot be a subnet address")
    end
    if last.broadcast == last
      errors.add("NetworkRange #{fullname}: #{last} cannot be a broadcast address")
    end

    # Now, verify that this range does not overlap with any other range

    NetworkRange.transaction do
      NetworkRange.all.each do |other|
        if other === first
          errors.add("NetworkRange #{fullname}: first address #{first.to_s} overlaps with range #{other.fullname}")
        end
        if other === last
          errors.add("NetworkRange #{fullname}: last address #{last.to_s} overlaps with range #{other.fullname}")
        end
        unless Network.check_conduit_sanity(conduit, other.conduit)
          errors.add("NetworkRange #{fullname}: Conduit mapping overlaps with network range #{other.fullname}")
        end
      end
    end
  end

end
