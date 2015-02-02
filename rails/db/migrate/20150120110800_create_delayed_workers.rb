
class CreateDelayedWorkers < ActiveRecord::Migration
  def change
    create_table(:delayed_workers) do |t|
      t.string :name
      t.timestamp :last_heartbeat_at
    end
    add_index(:delayed_workers, :name, unique: true)
  end
end
