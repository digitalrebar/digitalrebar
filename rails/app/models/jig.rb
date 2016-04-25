# Copyright 2014, Dell
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

=begin
  The Jig (base class) provides abstract interface for jig implementations. The general pattern
  is  that the base class provides class methods, that either:
   - locate the appropriate Jig instance and call the instance method on them
   - call the respective instance method on all jig's

  The exact pattern depends on the operation - some operations are 'broadcast' in nature,
  while some should only target a particular jig instance.
=end


class Jig < ActiveRecord::Base

  INTERNAL = ['test', 'noop', 'role-provided']

  after_create      :load_uuid

  def load_uuid
    self.reload
  end

  private :load_uuid

  #
  # Validate the name should unique
  # and that it starts with an alpha and only contains alpha,digits,hyphen,underscore
  #
  validates_uniqueness_of :name, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Name item must be unique")
  validates_format_of     :name, :with=> /\A[a-zA-Z][-_a-zA-Z0-9]*\z/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [-_a-zA-Z0-9]")

  has_many    :roles,     :primary_key=>:name, :foreign_key=>:jig_name
  belongs_to  :barclamp
  after_create :make_role_requires

  def die(str)
    Rails.logger.error(str)
    raise(str)
  end

  def self.active(jig)
    Jig.where(:name=>jig, :active=>true).length > 0
  end

  # OVERRIDE with actual methods
  def delete_node(node)
    Rails.logger.debug("jig.delete_node(#{node.name}) not implemented for #{self.class}.  This may be OK")
  end

  # expected to return JSON to be returned to the node
  def create_node(node)
    Rails.logger.debug("jig.create_node(#{node.name}) not implemented for #{self.class}.  This may be OK")
    {}
  end

  def client_role
    return nil unless client_role_name
    res = Role.find_by!(name: client_role_name)
    # Jig client roles must be implicit roles.
    raise "#{client_role_name} is not an implicit role!" unless res.implicit
    # Jig client roles cannot be implemented by the jig they implement
    # client-side functionality for.
    raise "#{client_role_name} is implemented by and requires #{name}!" if res.jig_name == name
    res
  end

  def on_disk_name
    name
  end

  # Gather all of the attribute data needed for a single noderole run.
  # It should be run to create whatever information will be needed
  # for the actual run before doing the actual run in a delayed job.
  # RETURNS the attribute data needed for a single noderole run.
  def stage_run(nr)
    nr.all_transition_data
  end

  # Run a single noderole.
  # The noderole must be in TRANSITION state.
  # This function is intended to be overridden by the jig subclasses,
  # and only used for debugging purposes.
  # Runs will be run in the background by the delayed_job information.
  def run(nr,data)
    raise "Cannot call run on the top-level Jig!"
  end

private

  def make_role_requires
    return true unless client_role_name
    roles.each do |r|
      RoleRequire.create!(role_id: r.id,
                          requires: client_role_name)
    end
  end
end

class NoopJig < Jig

  def stage_run(nr)
    return nr.all_my_data
  end

  def run(nr,data)
    return true
  end

end
