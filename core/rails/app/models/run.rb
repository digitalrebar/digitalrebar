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

  def log
    @@log ||= ActiveSupport::Logger.new("/var/log/rebar/runs.log")
    @@log
  end

  def run(run_id)
    log.info("Run: Starting queued job #{run_id}")
    job = Run.find(run_id) rescue nil
    unless job
      log.error("Run: Queued job #{run_id} vanished from the database!")
      destroy
      return
    end
    nr = nil
    # allows jigs to skip the active state as an exit condition
    # (allows for async jigs)
    mark_active = true
    begin
      NodeRole.transaction do
        nr = NodeRole.where(id: job.node_role_id).lock("FOR NO KEY UPDATE").first
        nr.transition!
        if nr.role.destructive && (nr.run_count > 0)
          log.info("Run: Queued job #{job.id} for #{nr.id} skipped due to destructiveness")
          nr.active! if nr.node.alive? && nr.node.available? && mark_active
          job.delete
          destroy
          return
        end
      end
      log.info("Run: Queued job #{job.id} for #{nr.name} running!")
      run_return = nr.jig.run(nr,job.run_data["data"])
      # mark the run as active unless it returns the :async symbol
      # when a jig returns this flag, then it needs to retry the node role for it to continue
      mark_active = run_return != :async
      log.info("Run: log for job #{job.id}:\n#{nr.runlog}\nLog for #{job.id} end")
      log.info("Run: Queued job #{job.id} for #{nr.id} finished, no exceptions raised.")
    rescue StandardError => e
      NodeRole.transaction do
        nr = NodeRole.where(id: job.node_role_id).lock("FOR NO KEY UPDATE").first
        nr.update(runlog: "#{nr.runlog}\n\n\n#{e.class.name}: #{e.message}\nBacktrace:\n#{e.backtrace.join("\n")}")
        nr.error!
        job.delete
        destroy
      end
      log.error("Run: log for job #{job.id}:\n#{nr.runlog}\n")
      log.error("Run: NodeRole #{nr.name} errored")
      log.error("Run: Queued job #{job.id} for #{nr.name} failed, exceptions raised.")
      log.error("#{e.class.name}: #{e.message}\nBacktrace:\n#{e.backtrace.join("\n")}")
      return
    end
    NodeRole.transaction do
      # Extract any new desired node attribs from the returned wall info
      nr.barclamp.attribs.where(role_id: nil).each do |a|
        val = a.simple_get(nr)
        next if val.nil? || a.simple_get(nr.node) == val
        a.set(nr.node,val)
      end
      # Handle updating our global idea about reservations if our wall has any.
      if (nr.wall["rebar_wall"]["reservations"] rescue nil)
        res = Hash.new
        nr.node.node_roles.each do |this_nr|
          # We do ourselves last
          next if nr.id == this_nr.id
          # Use the local copy of this noderole instead of the stale one from the db.
          next unless (this_nr.wall["rebar_wall"]["reservations"] rescue nil)
          res.deep_merge!(this_nr.wall["rebar_wall"]["reservations"])
        end
        # Do this one last and not from a stale copy
        res.deep_merge!(nr.wall["rebar_wall"]["reservations"])
        nr.node.hint_update({"reservations" => res})
      end
      log.info("Run: NodeRole #{nr.name} active")
      log.info("Run: Deleting finished job #{job.id}")
      nr.active! if nr.node.alive? && nr.node.available? && mark_active
      job.delete
      destroy
    end
    Run.run!
  end
end

class Run < ActiveRecord::Base

  after_create      :load_uuid

  def load_uuid
    self.reload
  end

  private :load_uuid

  belongs_to :node
  belongs_to :node_role
  belongs_to :que_job, foreign_key: 'job_id', primary_key: 'job_id'

  serialize :run_data

  scope :runnable,   -> { where("NOT running AND (node_id NOT IN (select node_id from runs where running))").order("id ASC")}
  scope :running,    -> { where(:running => true) }
  scope :running_on, ->(node_id) { running.where(:node_id => node_id) }

  def sort_id
    [node_role.cohort, node_role_id, id]
  end

  # Be careful here, it is disturbingly easy to break the one-and-only-run
  # on a node at a time invariant.
  def self.run_runnable
    # Nothing is allowed to touch the Runs table while we are moving runs from
    # runnable to running.
    Run.transaction(isolation: :serializable) do
      # Pick exactly one runnable run per node
      Run.find_by_sql(
        "UPDATE runs SET running = true
         WHERE id in (SELECT DISTINCT ON (node_id) id 
                      FROM runs 
                      WHERE NOT running 
                      AND node_id NOT IN (SELECT node_id 
                                          FROM runs 
                                          WHERE running) 
                      ORDER BY node_id, id  ASC)
         RETURNING *").each do |r|
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
    Run.transaction do
      queued_run = Run.where(node_role_id: nr.id, running: false).lock("FOR UPDATE").first
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
    run_runnable
  end

  def self.queue_status(arg)
    res = "Run: queue(#{arg}) \n"
    Run.all.order("id ASC").map do |r|
      res << " Run: #{r.id}: running:#{r.running}: #{r.node_role.name}: state #{r.node_role.state}"
      if r.running && r.job_id
        j = r.que_job
        if j
          res << " Job: #{r.job_id} queue #{j.queue} running on pid #{j.pid || "None"}"
        else
          res << " Job: #{r.job_id} not in a queue yet"
        end
      end
      res << "\n"
    end
    res
  end

  # Run up to maxruns runs, enqueuing runnable noderoles in TODO as it goes.
  def self.run!
    # Before we do anything else, perform some preventative maintenance on the
    # run queue to make sure we don't lose runs.
    Run.transaction do
      Run.find_by_sql("SELECT * FROM runs where 
                       (node_id NOT IN (select id from nodes)) OR
                       (node_role_id NOT IN (select id from node_roles)) OR
                       (running AND (job_id IS NOT NULL) AND (job_id NOT IN (select job_id from que_jobs))) FOR UPDATE SKIP LOCKED").each do |r|
        Rails.logger.error("Run: #{r.id} is invalid, deleting")
        Rails.logger.debug("Run: Invalid run: #{r.to_json}")
        r.delete
      end
    end
    # Handle runs that were created by Run.enqueue first.
    Run.run_runnable
    # Look for newly-runnable noderoles, and create Runs for them
    # if there is not already something running or runnable on the
    # node for the noderole.
    handled = 0
    Rails.logger.debug(Run.queue_status("start"))
    Run.transaction do
      # Find any runnable noderoles and see if they can be enqueued.
      # The logic here will only enqueue a noderole of the node does not
      # already have a noderole enqueued.
      # This can be optimized to use a SELECT DISTINCT ON in a similar
      # fashion to run_runnable, but the resultant sql would be much hairier.
      NodeRole.runnable.
        where("node_id NOT IN (select r.node_id FROM runs r)").
        order("cohort ASC, id ASC").lock("FOR UPDATE SKIP LOCKED").each do |nr|
        next if Run.where(node_id: nr.node_id).count != 0
        Rails.logger.info("Run: Creating new Run for #{nr.name}")
        # Take a snapshot of the data we want to hand to the jig's run
        # method.  We do this so that the jig gets fed data that is
        # consistent for this point in time, as opposed to picking up
        # whatever is lying around when delayed_jobs gets around to
        # actually doing its thing, which may not be what we expect.
        Run.create!(node_id: nr.node_id, node_role_id: nr.id,
                    queue: "NodeRoleRunner",
                    run_data: {"data" => nr.jig.stage_run(nr)})
        handled += 1
      end
    end
    # Second pass through runnable runs, poking freshly-created ones this time.
    Run.run_runnable
    Rails.logger.info("Run: #{handled} handled this pass, #{Run.running.count} in delayed_jobs")
    # log queue state
    Rails.logger.debug(Run.queue_status("end"))
    return handled
  end
end
