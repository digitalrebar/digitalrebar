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
#

class Run < ActiveRecord::Base

  belongs_to :node
  belongs_to :node_role

  serialize :run_data

  scope :runnable,   -> { where(:running => false).where("node_id NOT IN (select node_id from runs where running)").order("id ASC")}
  scope :running,    -> { where(:running => true) }
  scope :running_on, ->(node_id) { running.where(:node_id => node_id) }

  def sort_id
    [node_role.cohort, node_role_id, id]
  end

  def self.empty?
    Run.locked_transaction do
      Run.all.count == 0
    end
  end

  # Be careful here, it is disturbingly easy to break the one-and-only-run
  # on a node at a time invariant.
  def self.run_runnable
    # Nothing is allowed to touch the Runs table while we are moving runs from
    # runnable to running.
    Run.locked_transaction do
      Run.runnable.each do |j|
        next if Run.running.find_by(node_id: j.node_id)
        j.update!(running: true)
      end
    end

    if Rails.env.development?
      Run.running.where(delayed_job_id: nil).order("id ASC").each do |j|
        # If we are in dev mode, just run the run directly.
        # This happens outside a transaction because run_job expects that
        # it starts running outside a transaction, and it will break
        # otherwise.
        j.node_role.jig.run_job(j)
      end
      return
    end
    # Grab all the runs marked as running that do not have a
    # delayed_job assigned to them, get a row lock on the individual
    # rows, and feed them into delayed_jobs.  The row locks should
    # prevent concurrent access to the same rows across different
    # transactions.  Note that we look at all the runs that are marked
    # as running, not just the ones we marked as running above.  This
    # should prevent the case where we marked a run as running, and
    # then crashed.
    Run.transaction do
      Run.running.where(delayed_job_id: nil).order("id ASC").lock(true).each do |j|
        Rails.logger.info("Run: Sending run #{j.id}: #{j.node_role.name} to delayed_jobs queue #{j.queue}")
        j.update!(delayed_job_id: j.node_role.jig.delay(queue: j.queue).run_job(j).id)
      end
    end
  end

  # Queue up a run to run.
  # Run.enqueue should only be called when you want to enqueue a noderole
  # that is in a state other than TODO, as those will be picked up by Run.run!
  # The main callers of this should mostly be events called from role triggers.
  def self.enqueue(nr)
    raise "cannot enqueue a nil node_role!" if nr.nil?
    Run.locked_transaction do
      queued_run = Run.find_by(node_role_id: nr.id, running: false)
      if queued_run
        Rails.logger.debug("Run: Updating enqueued #{queued_run.id} with new run data for #{nr.name}")
        queued_run.update!(run_data: {"data" => nr.jig.stage_run(nr)})
        return
      end
      unless nr.runnable? && (nr.active? || nr.todo? || nr.transition?)
        Rails.logger.debug("Run: Not enqueing #{nr.name}")
        return
      end
      Rails.logger.info("Run: Enqueing Run for #{nr.name}")
      Run.create!(node_id: nr.node_id,
                  node_role_id: nr.id,
                  queue: "HighPriorityRunner",
                  run_data: {"data" =>  nr.jig.stage_run(nr)})
    end
    run!
  end

  def self.queue(arg)
    r = "Run: queue(#{arg}) "
    Run.all.order("id ASC").map do |j|
      r << " Run: #{j.id}: running:#{j.running}: #{j.node_role.name}: state #{j.node_role.state}"
      if j.running
        dj = Delayed::Job.find(j.delayed_job_id)
        r << " Delayed_job: #{dj.id} queue #{dj.queue} backend #{dj.locked_by || "None"}"
      end
      r << "\n"
    end
    r
  end

  # Run up to maxruns runs, enqueuing runnable noderoles in TODO as it goes.
  def self.run!(maxruns=10)
    runs = {}
    # Before we do anything else, perform some preventative maintenance on the
    # run queue to make sure we don't lose runs.
    Run.locked_transaction do
      dj_backends = Hash.new
      Run.all.each do |r|
        kill_run = false
        if r.node.nil?
          # The node that the run should have been perfomed on is gone.
          # If this happens, something has gone Horribly Wrong, or
          # BDD is playing very fast and loose with our referential integrity.
          Rails.logger.error("Run: #{r.id} is present for a nonexistent node.")
          kill_run = true
        end
        if r.node_role.nil?
          # The noderole that requested the run is gone.
          # Also either something gone Horribly Wrong or BDD murderating
          # noderoles.
          Rails.logger.error("Run: #{r.id} is present for a missing noderole.")
          kill_run = true
        end
        if r.running && r.delayed_job_id
          dj = Delayed::Job.find(r.delayed_job_id)
          if dj.nil?
            # The run thinks it should be running, the delayed_job backing it
            # has gone away.
            Rails.logger.error("Run: #{r.id} is running, but missing delayed_job #{r.delayed_job_id}")
            kill_run = true
          elsif dj.locked_by
            if dj_backends[dj.locked_by]
              Rails.logger.fatal("Run: More than one running run assigned to a backend! Email victor.")
              Rails.logger.fatal(Run.queue("deadlocked"))
              raise "Unrecoverable error in Run.run! Email Victor, and save the logs."
            end
            dj_backends[dj.locked_by] = true
          end
        end
        next unless kill_run
        if r.node_role
          r.node_role.runlog = "Run: #{r.inspect} is invalid.  Deleting it."
          r.node_role.error!
        end
        r.delete
      end
    end
    # Handle runs that were created by Run.enqueue first.
    Run.run_runnable
    # Look for newly-runnable noderoles, and create Runs for them
    # if there is not already something running or runnable on the
    # node for the noderole.
    Run.locked_transaction do
      Rails.logger.debug(Run.queue("start"))
      running = Run.running.count
      # Find any runnable noderoles and see if they can be enqueued.
      # The logic here will only enqueue a noderole of the node does not
      # already have a noderole enqueued.
      NodeRole.runnable.order("cohort ASC, id ASC").each do |nr|
        break if runs.length + running >= maxruns
        next if runs[nr.node_id] || Run.exists?(node_id: nr.node_id)
        Rails.logger.info("Run: Creating new Run for #{nr.name}")
        # Take a snapshot of the data we want to hand to the jig's run
        # method.  We do this so that the jig gets fed data that is
        # consistent for this point in time, as opposed to picking up
        # whatever is lying around when delayed_jobs gets around to
        # actually doing its thing, which may not be what we expect.
        runs[nr.node_id] = Run.create!(node_id: nr.node_id,
                                       node_role_id: nr.id,
                                       queue: "NodeRoleRunner",
                                       run_data: {"data" => nr.jig.stage_run(nr)})
      end
    end
    # Second pass through runnable runs, poking freshly-created ones this time.
    Run.run_runnable
    Rails.logger.info("Run: #{runs.length} handled this pass, #{Run.running.count} in delayed_jobs")
    # log queue state
    Rails.logger.debug(Run.queue("end"))
    return runs.length
  end
end
