class AddNotesAttribs < ActiveRecord::Migration
  def change
    change_table :nodes do |t|
      t.json :notes, null: false, default: { expr: "'{}'::json" }
    end

    change_table :roles do |t|
      t.json :notes, null: false, default: { expr: "'{}'::json" }
    end

    change_table :deployment_roles do |t|
      t.json :notes, null: false, default: { expr: "'{}'::json" }
    end

    change_table :node_roles do |t|
      t.json :notes, null: false, default: { expr: "'{}'::json" }
    end
  end
end
