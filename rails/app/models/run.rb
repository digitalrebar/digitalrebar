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

  def self._run(j)
    if Run.running.find_by(queue: j.queue, node_id: j.node_id).nil?
      Rails.logger.info("Run: Sending job #{j.id}: #{j.node_role.name} to delayed_jobs queue #{j.queue}")
      # dev mode not starting queued jobs, we need to skip queuing for now
      j.running = true
      j.save!
      if Rails.env.development?
        j.node_role.jig.run_job(j)
      else
        j.delayed_job_id = j.node_role.jig.delay(queue: j.queue).run_job(j).id
        j.save!
      end
    end
  end

  # Queue up a job to run.
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

  # Run up to maxjobs jobs, enqueuing runnable noderoles in TODO as it goes.
  def self.run!(maxjobs=10)
    jobs = {}
    Run.locked_transaction do
      # Error out any running jobs that do not have a delayed_job backing them.
      Run.running.each do |r|
        next if Delayed::Job.find(r.delayed_job_id)
        r.node_role.runlog = "Run: delayed_job #{r.delayed_job_id} for run #{r.id} of noderole #{r.node_role.id} vanished!"
        r.node_role.error!
        r.delete
      end
      queue = Run.all.map{|j|"Job: #{j.id}: running:#{j.running}: #{j.node_role.name}: state #{j.node_role.state} "}
      Rails.logger.debug("Run: Queue: (start) #{queue}")
      running = Run.running.count
      # Look for enqueued runs and schedule at most one per node to go.
      Run.runnable.each do |j|
        Run._run(j)
      end

      # Find any runnable noderoles and see if they can be enqueued.
      # The logic here will only enqueue a noderole of the node does not
      # already have a noderole enqueued.
      NodeRole.runnable.order("cohort ASC, id ASC").each do |nr|
        break if jobs.length + running >= maxjobs
        next if jobs[nr.node_id] || Run.exists?(node_id: nr.node_id)
        Rails.logger.info("Run: Creating new Run for #{nr.name}")
        jobs[nr.node_id] = Run.create!(node_id: nr.node_id,
                                       node_role_id: nr.id,
                                       queue: "NodeRoleRunner",
                                       # Take a snapshot of the data we want to hand to the jig's run method.
                                       # We do this so that the jig gets fed data that is consistent for this point
                                       # in time, as opposed to picking up whatever is lying around when delayed_jobs
                                       # gets around to actually doing its thing, which may not be what we expect.

                                       run_data: {"data" => nr.jig.stage_run(nr)})
      end
      return if jobs.length == 0
      # Now that we have things that are runnable, loop through them to see
      # what we can actually run.
      jobs.values.each do |j|
        Run._run(j)
      end
    end
    Rails.logger.info("Run: #{jobs.length} handled this pass, #{Run.running.count} in delayed_jobs")
    begin
      # log queue state
      Rails.logger.debug("Run: Queue: (end) #{Run.all.map{|j|"Job: #{j.id}: running:#{j.running}: #{j.node_role.name}: state #{j.node_role.state}"}}")
    rescue
      # catch node_role is nil (exposed in simulator runs)
      Run.all.each { |j| raise "you cannot run job #{j.id} with missing node #{j.node_id} and node_role #{j.node_role_id} information.  This is likely a garbage collection issue!" if j.node_role.nil? }
    end
    return jobs.length
  end
end
