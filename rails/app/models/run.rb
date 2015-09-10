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

class QueJob < ActiveRecord::Base

  def pid
   # Short version: Jobs in Que are locked by taking an advisory lock
   # using their ID as the lock token. The fact that this lock was
   # granted along with the postgres backend worker who holds it is
   # recorded in the pg_locks view along with the pg_stat_activity
   # view.
    rows = ActiveRecord::Base.connection.select_all("
select l.pid
from (
    select
    (l.classid::bigint << 32) + l.objid::bigint AS job_id,
    pg.pid AS pid
    from pg_locks l
    inner join pg_stat_activity pg
    using (pid)
    where l.locktype = 'advisory'
) l
inner join que_jobs j
using (job_id)
where job_id = #{job_id}")
    return nil if rows.empty?
    return rows[0]["pid"]
  end

  def locked?
    !!pid
  end
end

class NodeRoleRun < Que::Job

  def run(run_id)
    Rails.logger.info("Run: Starting queued job #{run_id}")
    job = Run.find(run_id) rescue nil
    unless job
      Rails.logger.error("Run: Queued job #{run_id} vanished from the database!")
      destroy
      return
    end
    nr = job.node_role
    Rails.logger.info("Run: Queued job #{run_id} for #{nr.name} found.")
    to_error = false
    # allows jigs to skip the active state as an exit condition
    # (allows for async jigs)
    mark_active = true
    begin
      nr.transition!
      unless nr.role.destructive && (nr.run_count > 0)
        Rails.logger.info("Run: Queued job #{job.id} for #{nr.name} running!")
        run_return = nr.jig.run(nr,job.run_data["data"])
        # mark the run as active unless it returns the :async symbol
        # when a jig returns this flag, then it needs to retry the node role for it to continue
        mark_active = run_return != :async
        Rails.logger.info("Run: Queued job #{job.id} for #{nr.name} finished, no exceptions raised.")
      else
        Rails.logger.info("Run: Queued job #{job.id} for #{nr.name} skipped due to destructiveness")
      end
      # Extract any new desired node attribs from the returned wall info
      nr.barclamp.attribs.where(role_id: nil).each do |a|
        val = a.get(nr)
        next if val.nil?
        a.set(nr.node,val)
      end
      # Handle updating our global idea about reservations if our wall has any.
      if (nr.wall["rebar_wall"]["reservations"] rescue nil)
        res = Hash.new
        nr.node.node_roles.each do |this_nr|
          next unless (this_nr.wall["rebar_wall"]["reservations"] rescue nil)
          res.deep_merge!(this_nr.wall["rebar_wall"]["reservations"])
          end
        nr.node.hint_update({"reservations" => res})
      end
    rescue StandardError => e
      to_error = true
      NodeRole.transaction do
        nr.update!(runlog: "#{e.class.name}: #{e.message}\nBacktrace:\n#{e.backtrace.join("\n")}")
      end
      Rails.logger.info("Run: Queued job #{job.id} for #{nr.name} failed, exceptions raised.")
      Rails.logger.error("#{e.class.name}: #{e.message}\nBacktrace:\n#{e.backtrace.join("\n")}")
    ensure
      NodeRole.transaction do
        nr.save!
        Rails.logger.info("Run: Deleting finished job #{job.id}")
        job.delete
        if to_error
          nr.error!
        else
          # Only go to active if the node is still alive -- the jig may
          # have marked it as not alive.
          nr.active! if nr.node.alive? && nr.node.available? && mark_active
        end
        destroy
      end
    end
  end
end

class Run < ActiveRecord::Base

  audited

  belongs_to :node
  belongs_to :node_role
  belongs_to :que_job, foreign_key: 'job_id', primary_key: 'job_id'

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
      Run.runnable.each do |r|
        next if Run.running.find_by(node_id: r.node_id)
        r.update!(running: true)
      end
      Run.running.where(job_id: nil).order("id ASC").lock.each do |r|
        next if Run.running.find_by(["node_id = ? AND job_id IS NOT NULL", r.node_id])
        job = NodeRoleRun.enqueue(r.id, queue: r.queue)
        r.update!(job_id: job.attrs["job_id"])
      end
    end
    Que.job_stats.each do |j|
      Rails.logger.info("Run: Queue #{j["queue"]}: #{j["count"]} jobs, #{j["count_working"]} running.")
    end
    Que.worker_states.each do |w|
      Rails.logger.info("Run: Que worker for run #{w["args"][0]} in queue #{w["queue"]} running against database backend #{w["pg_backend_pid"]}")
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

  def self.queue_status(arg)
    res = "Run: queue(#{arg}) \n"
    Run.all.order("id ASC").map do |r|
      res << " Run: #{r.id}: running:#{r.running}: #{r.node_role.name}: state #{r.node_role.state}"
      if r.running && r.job_id
        j = r.que_job
        res << " Job:  #{r.job_id} queue #{j.queue} running on pid #{j.pid || "None"}"
      end
      res << "\n"
    end
    res
  end

  # Run up to maxruns runs, enqueuing runnable noderoles in TODO as it goes.
  def self.run!
    runs = {}
    # Before we do anything else, perform some preventative maintenance on the
    # run queue to make sure we don't lose runs.
    Run.locked_transaction do
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
        if r.running && r.job_id
          dj = r.que_job
          if dj.nil?
            # The run thinks it should be running, the delayed_job backing it
            # has gone away.
            Rails.logger.error("Run: #{r.id} is running, but missing job #{r.job_id}")
            kill_run = true
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
      Rails.logger.debug(Run.queue_status("start"))
      # Find any runnable noderoles and see if they can be enqueued.
      # The logic here will only enqueue a noderole of the node does not
      # already have a noderole enqueued.
      NodeRole.runnable.order("cohort ASC, id ASC").each do |nr|
        next if Run.exists?(node_id: nr.node_id)
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
    Rails.logger.debug(Run.queue_status("end"))
    return runs.length
  end
end
