class MakeRangesSpecifyDhcpParameters < ActiveRecord::Migration

  def self.up
    add_column :network_ranges, :anon_lease_time, :integer, default: 60, null: false # 60 seconds
    add_column :network_ranges, :bound_lease_time, :integer, default: 2592000, null: false # 30 days
    add_column :network_ranges, :allow_anon_leases, :bool, default: false, null: false
    add_column :network_ranges, :allow_bound_leases, :bool, default: false, null: false
  end
  
end
