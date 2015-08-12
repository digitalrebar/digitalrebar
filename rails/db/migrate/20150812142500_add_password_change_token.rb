class AddPasswordChangeToken < ActiveRecord::Migration

  def change
    create_table :password_change_tokens do |t|
      t.belongs_to :user
      t.text :key, null: false, index: true
      t.text :token, null: false
      t.timestamps
    end
  end

end
