class AddQue < ActiveRecord::Migration
  def self.up
    # The current version as of this migration's creation.
    Que.migrate! :version => 3

    change_table :runs do |t|
      t.integer   :job_id, references: nil
    end
  end

  def self.down
    # Completely removes Que's job queue.
    Que.migrate! :version => 0
  end
end
