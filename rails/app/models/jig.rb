# Copyright 2014, Dell
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

=begin
  The Jig (base class) provides abstract interface for jig implementations. The general pattern
  is  that the base class provides class methods, that either:
   - locate the appropriate Jig instance and call the instance method on them
   - call the respective instance method on all jig's

  The exact pattern depends on the operation - some operations are 'broadcast' in nature, 
  while some should only target a particular jig instance.
=end


class Jig < ActiveRecord::Base

  # 
  # Validate the name should unique 
  # and that it starts with an alpha and only contains alpha,digits,hyphen,underscore
  #
  validates_uniqueness_of :name, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Name item must be unique")
  validates_format_of     :name, :with=> /\A[a-zA-Z][-_a-zA-Z0-9]*\z/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [-_a-zA-Z0-9]")

  has_many    :roles,     :primary_key=>:name, :foreign_key=>:jig_name
  belongs_to  :barclamp
  after_create :make_role_requires

  def die(str)
    Rails.logger.error(str)
    raise(str)
  end

  def self.active(jig)
    Jig.where(:name=>jig, :active=>true).length > 0
  end

  # OVERRIDE with actual methods
  def delete_node(node)
    Rails.logger.debug("jig.delete_node(#{node.name}) not implemented for #{self.class}.  This may be OK")
  end

  # expected to return JSON to be returned to the node
  def create_node(node)
    Rails.logger.debug("jig.create_node(#{node.name}) not implemented for #{self.class}.  This may be OK")
    {}
  end

  def client_role
    crn = client_role_name
    return nil if crn.nil?
    res = Role.find_by(name: crn)
    # Jig client roles must be implicit roles.
    raise "#{crn} is not an implicit role!" unless res.implicit
    # Jig client roles cannot be implemented by the jig they implement
    # client-side functionality for.
    raise "#{crn} is implemented by and requires #{name}!" if res.jig_name == name
    res
  end

  def on_disk_name
    name
  end

  # Gather all of the attribute data needed for a single noderole run.
  # It should be run to create whatever information will be needed
  # for the actual run before doing the actual run in a delayed job.
  # RETURNS the attribute data needed for a single noderole run.
  def stage_run(nr)
    res = {}
    # Figure out which attribs will be satisfied from node data vs.
    # which will be satisfied from noderoles.
    NodeRole.transaction do
      node_req_attrs = nr.role.role_require_attribs.select do |rrr|
        attr = rrr.attrib
        raise("RoleRequiresAttrib: Cannot find required attrib #{rrr.attrib_name}") if attr.nil?
        attr.role_id.nil?
      end
      # For all the node attrs, resolve them.  Prefer hints.
      # Start with the node data.
      node_req_attrs.each do |req_attr|
        Rails.logger.info("Jig: Adding node attribute #{req_attr.attrib_name} to attribute blob for #{nr.name} run")
        res.deep_merge!(req_attr.get(nr.node))
      end
      # Next, do the same for the attribs we want from a noderole.
      nr.parent_attrib_links.each do |al|
        res.deep_merge!(al.attrib.extract(al.parent.all_committed_data))
      end
      # And all the noderole data from the parent noderoles on this node.
      # This needs to eventaully go away once I figure ot the best way to pull
      # attrib data that hsould always be present on a node.
      nr.all_parents.where(node_id: nr.node.id).each do |pnr|
        res.deep_merge!(pnr.all_committed_data)
      end
      # Add this noderole's attrib data.
      Rails.logger.info("Jig: Merging attribute data from #{nr.name} for jig run.")
      res.deep_merge!(nr.all_committed_data)
      # Add information about the resource reservations this node has in place
      unless nr.node.discovery["reservations"]
      res["crowbar_wall"] ||= Hash.new
        res["crowbar_wall"]["reservations"] = nr.node.discovery["reservations"]
      end
      # Add any hints.
      res["hints"] = nr.node.hint
      # Add quirks
      res["quirks"] = nr.node.quirks
      # And we are done.
    end
    res
  end

  def finish_run(nr)
    NodeRole.transaction do
      nr.save!
      # Handle updating our global idea about reservations if our wall has any.
      if (nr.wall["crowbar_wall"]["reservations"] rescue nil)
        res = Hash.new
        nr.node.node_roles.each do |this_nr|
          next unless (this_nr.wall["crowbar_wall"]["reservations"] rescue nil)
          res.deep_merge!(this_nr.wall["crowbar_wall"]["reservations"])
        end
        nr.node.discovery.merge({"reservations" => res})
      end
    end
  end

  # Run a single noderole.
  # The noderole must be in TRANSITION state.
  # This function is intended to be overridden by the jig subclasses,
  # and only used for debugging purposes.
  # Runs will be run in the background by the delayed_job information.
  def run(nr,data)
    raise "Cannot call run on the top-level Jig!"
  end

  def run_job(job)
    loop do
      nr = job.node_role
      begin
        nr.transition!
        unless nr.role.destructive && (nr.run_count > 0)
          Rails.logger.info("Run: Running job #{job.id} for #{nr.name}")
          nr.jig.run(nr,job.run_data["data"])
          Rails.logger.debug("Run: Finished job #{job.id} for #{nr.name}, no exceptions raised.")
        else
          Rails.logger.info("Run: Skipping run for job #{job.id} for #{nr.name} due to destructiveness")
        end
        # Only go to active if the node is still alive -- the jig may
        # have marked it as not alive.
        nr.active! if nr.node.alive? && nr.node.available?
      rescue Exception => e
        NodeRole.transaction do
          nr.update!(runlog: "#{e.class.name}: #{e.message}\nBacktrace:\n#{e.backtrace.join("\n")}")
          nr.error!
        end
        Rails.logger.debug("Run: Finished job #{job.id} for #{nr.name}, exceptions raised.")
        Rails.logger.error("#{e.class.name}: #{e.message}\nBacktrace:\n#{e.backtrace.join("\n")}")
      ensure
        finish_run(nr)
        Run.locked_transaction do
          Rails.logger.debug("Run: Deleting finished job #{job.id} for #{nr.name}")
          job.delete
          # Try to steal work for this node if we can.
          job = nil
          unless Run.exists?(node_id: nr.node_id, running: true)
            # This will ensure that queued noderoles get processed
            # in proper dependency order.
            job = Run.where(node_id: nr.node_id).runnable.first
            if job
              # Pick the latest job with the same noderole as the job we just found, and
              # delete the previous instances.  This ensures that we do not waste time running
              # intermediate roles where we do not need to.
              Rails.logger.info("Run: Stole preexisting run #{job.id}")
              job.running = true
              job.save!
            else
              # We did not find an already-enqueued run, so see if we can make
              # one from a handy runnable NodeRole.
              nr = NodeRole.where(node_id: nr.node_id).runnable.order("cohort ASC").first
              if nr
                Rails.logger.info("Run: Stealing #{nr.name}")
                job = Run.create!(node_id: nr.node_id,
                                  node_role_id: nr.id,
                                  running: true,
                                  run_data: {"data" => nr.jig.stage_run(nr)})
              end
            end
          end
        end
      end
      break unless job
    end
    Run.run!
  end

private

  def make_role_requires
    return true unless client_role_name
    roles.each do |r|
      RoleRequire.create!(role_id: r.id,
                          requires: client_role_name)
    end
  end
end

class NoopJig < Jig

  def stage_run(nr)
    return nr.all_my_data
  end

  def run(nr,data)
    return true
  end

end
