class AddAttirbDefault < ActiveRecord::Migration
  def change
    change_table :attribs do |t|
      t.json   :default, default: { expr: "'{\"value\": null}'::json" }
    end
  end
end
