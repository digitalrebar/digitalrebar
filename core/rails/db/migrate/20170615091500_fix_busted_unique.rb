# Copyright 2017, RackN
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
class FixBustedUnique < ActiveRecord::Migration

  def self.up
    # Don't bother fixing up uuids, but take a stab at everything else.

    # Kill any duplicates that may have snuck in
    EventSink.all.pluck(:endpoint).uniq.each do |ep|
      sinks = EventSink.where(endpoint: ep).order("id DESC").pluck(:id)
      next if sinks.length == 1
      selectors = EventSelector.where(event_sink_id: sinks)
      sink = sinks.shift
      selectors.update_all(sink_id: sink)
      EventSink.where(id: sinks).delete_all
    end

    Capability.all.pluck(:name).uniq.each do |name|
      caps = Capability.where(name: name).order("id DESC").pluck(:id)
      next if caps.length == 1
      utcs = UserTenantCapability.where(capability_id: caps)
      cap = caps.to_a.shift
      utcs.update_all(capability_id: cap)
      Capability.where(id: caps).delete_all
    end

    Profile.all.pluck(:name).uniq.each do |name|
      profiles = Profile.where(name: name).order("id DESC").pluck(:id)
      next if profiles.length == 1
      profiles.shift
      Profile.where(id: profiles).delete_all
    end

    add_index :event_sinks, :uuid, unique: true
    add_index :event_sinks, :endpoint, unique: true
    add_index :event_selectors, :uuid, unique: true
    add_index :tenants, :uuid, unique: true
    add_index :tenants, :name, unique: true
    add_index :capabilities, :uuid, unique: true
    add_index :capabilities, :name, unique: true
    add_index :user_tenant_capabilities, :uuid, unique: true
    add_index :profiles, :uuid, unique: true
    add_index :profiles, :name, unique: true
  end
end
