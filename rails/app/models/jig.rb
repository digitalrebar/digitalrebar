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
    nr.all_transition_data
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
    nr = job.node_role
    to_error = false
    mark_active = true  # allows jigs to skip the active state as an exit condition (allows for async jigs)
    begin
      nr.transition!
      unless nr.role.destructive && (nr.run_count > 0)
        Rails.logger.info("Run: Running job #{job.id} for #{nr.name}")
        run_return = nr.jig.run(nr,job.run_data["data"])
        # mark the run as active unless it returns the :async symbol
        # when a jig returns this flag, then it needs to retry the node role for it to continue
        mark_active = run_return != :async
        Rails.logger.debug("Run: Finished job #{job.id} for #{nr.name}, no exceptions raised.")
      else
        Rails.logger.info("Run: Skipping run for job #{job.id} for #{nr.name} due to destructiveness")
      end
    rescue StandardError => e
      to_error = true
      NodeRole.transaction do
        nr.update!(runlog: "#{e.class.name}: #{e.message}\nBacktrace:\n#{e.backtrace.join("\n")}")
      end
      Rails.logger.debug("Run: Finished job #{job.id} for #{nr.name}, exceptions raised.")
      Rails.logger.error("#{e.class.name}: #{e.message}\nBacktrace:\n#{e.backtrace.join("\n")}")
    ensure
      finish_run(nr)
      Run.locked_transaction do
        Rails.logger.debug("Run: Deleting finished job #{job.id} for #{nr.name}")
        job.delete
      end
      if to_error
        nr.error!
      else
        # Only go to active if the node is still alive -- the jig may
        # have marked it as not alive.
        nr.active! if nr.node.alive? && nr.node.available? && mark_active
      end
    end
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
