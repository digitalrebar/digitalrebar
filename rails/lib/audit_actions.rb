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
#

class AuditActions

  # Audit looks like this:
  # id:              int    # database index
  # auditable_id:    int    # Not sure - seems like index
  # auditable_type:  string # Class audited
  # associated_id:   int    # parent id of association
  # associated_type: string # Class of associated type
  # user_id:         int    # Id of user taking action
  # user_type:       string # Class?
  # username:        string # Name of user taking action
  # action:          string # "create", "update", "destroy"
  # audit_changes:   hash   # values that changed in action
  # version:         int    # version of object at time of change
  # comment:         string # Comment on what change if provided
  # remote_address:  ??     # ??
  # request_uuid:    string # uuid representing action (chains audits together)
  # created_at:      time   # creation time
  def self.to_amqp

    # Make sure there an instance of me in the table.
    AuditTracker.transaction do
      AuditTracker.create!(tracker: 'AMQP') unless AuditTracker.find_by_tracker('AMQP')
    end

    # Run forever spewing events
    while true do
      audit = nil
      AuditTracker.transaction do
        me = AuditTracker.find_by_tracker('AMQP')

        audit = Audited::Adapters::ActiveRecord::Audit.unscoped.where("id > ?", me.processed).limit(1).order("id ASC").first
        if audit
          Publisher::publish_event(audit.auditable_type, audit.action, audit)
          me.processed = audit.id
          me.save!
        end
      end
      sleep(5) unless audit
    end
  end

  def self.purge
    last_seen_by_all = AuditTracker.order("processed ASC").first.processed rescue 0

    count = 0
    time = Time.now - 7.days
    # Only destroy things older than 30 days and seen by all trackers.
    Audited::Adapters::ActiveRecord::Audit.unscoped.where("id <= ? AND created_at < ?", last_seen_by_all, time).each do |audit|
      audit.destroy!
      count += 1
    end

    puts "Purged #{count} entries"
  end

end
