# Copyright 2014 Victor Lowther
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

class BarclampRaid::RaidHammer < Hammer

  # Special class that will write out the string to the
  # node role runlog as it goes.
  class NodeRoleLogger < StringIO
    def initialize(nr)
      super()
      @node_role = nr
    end

    def <<(s)
      super(s)
      return unless @node_role
      NodeRole.transaction do
        ns = @node_role.runlog + s
        @node_role.update!(runlog: ns)
      end
    end
  end

  def self.drivers(candidate_node, logger)
    res = []
    Attrib.get('raid-drivers',candidate_node).each do |driver|
      next unless Object.qualified_const_defined?(driver["type"])
      driver = Object.qualified_const_get(driver["type"]).new(candidate_node,logger,driver)
      next unless driver.useable?
      res << driver
    end
    res
  end

  def self.probe(candidate_node)
    drivers(candidate_node).empty?
  end

  def drivers(logger)
    self.class.drivers(node, logger)
  end

  # Compares a configed volume versus a wanted volume
  #
  # @param cv [BarclampRaid::Volume] configured volume
  # @param wv [Config Request Volume] requested volume
  # @return [Boolean] true if volumes are the same
  def same_volume(cv, wv)
    return false unless cv
    return false unless wv
    return false if cv['name'] != wv['name']
    return false if cv['raid_level'] != wv['raid_level']
    return false if cv['disks'].length != wv['disks']
    return true
  end

  # Actions for this hammer
  #
  # @return [Hash] hash of action for this class of hammer (:raid)
  def actions
    { raid: [:detect, :converge] }
  end

  # Detect controllers
  #
  # @return [Array<BarclampRaid::Driver>]
  def detect(nr)
    logger = NodeRoleLogger.new(nr)
    logger << "Detecting Raid Configuration\n"
    ans = drivers(logger).map{|d|d.controllers}.flatten
    ans
  end

  # Apply the raid configuration from the provided noderole and update
  # the output log.
  #
  # @param nr [NodeRole]param noderole containing raid configuration
  # @return [Array<Volumes>] The current volumes on the system
  def converge(nr)
    logger = NodeRoleLogger.new(nr)
    logger << "Converging Raid Configuration\n"

    wanted_config = Attrib.get('raid-wanted-volumes',nr) || []
    # Figure out what our current volumes are
    current_controllers = drivers(logger).map{|d|d.controllers}.flatten
    current_volumes = []
    current_controllers.each do |c|
      current_volumes += c.volumes
    end

    # Figure out what to kill and what to keep.
    current_volume_hash = (current_volumes.map{|v|[v["name"], v]}.to_h rescue {})
    wanted_volume_hash = (wanted_config.map{|v|[v["name"], v]}.to_h rescue {})

    obsolete_volumes = current_volumes.reject{|v|same_volume(v, wanted_volume_hash[v['name']])}
    new_volumes = wanted_config.reject{|v|same_volume(current_volume_hash[v['name']], v)}

    # kill any volumes we no longer care about
    obsolete_volumes.each do |v|
      logger << "Deleting RAID volume #{v.inspect}\n"
      begin
        v.delete_vd
        current_volumes.delete(v)
      rescue => e
        Rails.logger.error("Unable to delete #{v}")
        Rails.logger.error("Error was: #{e.message}")
        logger << "Unable to delete #{v}\n"
        logger << "Error was: #{e.message}\n"
      end
    end
    # Some day, modify the configuration of changed ones, if their requested config changed.
    # Crete new volumes.
    new_volumes.each do |v|
      new_vol = nil
      current_controllers.each do |c|
        begin
          logger << "Creating new volume #{v}\n"
          new_vol = c.create_vd(v)
        rescue => e
          Rails.logger.info("Could not create #{v} on #{c.name}")
          Rails.logger.info("Error was: #{e.message}")
          logger << "Could not create #{v} on #{c.name}\n"
          logger << "Error was: #{e.message}\n"
          next
        end
        logger << "Volume #{v} created on #{c.name}\n"
        break
      end
      if new_vol
        current_volumes << new_vol
      else
        Rails.logger.error("Unable to create #{v} on any raid controller!\n")
        logger << "Unable to create #{v} on any raid controller!\n"
      end
    end

    current_volumes
  end
end
