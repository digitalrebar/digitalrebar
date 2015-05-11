class InstallAudited < ActiveRecord::Migration
  def self.up
    create_table :audits, :force => true do |t|
      t.column :auditable_id, :integer, references: nil
      t.column :auditable_type, :text
      t.column :associated_id, :integer, references: nil
      t.column :associated_type, :text
      t.column :user_id, :integer, references: nil
      t.column :user_type, :text
      t.column :username, :text
      t.column :action, :text
      t.column :audited_changes, :text
      t.column :version, :integer, :default => 0
      t.column :comment, :text
      t.column :remote_address, :text
      t.column :request_uuid, :text, index: true
      t.column :created_at, :datetime, index: true
    end

    add_index :audits, [:auditable_id, :auditable_type], :name => 'auditable_index'
    add_index :audits, [:associated_id, :associated_type], :name => 'associated_index'
    add_index :audits, [:user_id, :user_type], :name => 'user_index'
  end

  def self.down
    drop_table :audits
  end
end
