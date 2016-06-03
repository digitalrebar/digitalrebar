# Copyright 2016, RackN
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

class RolePreceed < ActiveRecord::Base

  belongs_to      :role
  belongs_to      :child, class_name: Role, foreign_key: "child_role_id"

  validate :enforce_acyclic, on: :create

  after_create :resolve_preceeds

  def resolved?
    !child_role_id.nil?
  end

  def resolve!
    return if resolved?
    Rails.logger.info("RolePreceed: Trying to resolve #{preceeds} to a Role for #{role.name}")
    r = Role.find_by(name: self.preceeds)
    return unless r
    Rails.logger.info("RolePreceed: #{preceeds} resolves to role ID #{r.id}")
    update_column(:child_role_id, r.id)
    child.update_cohort
  end


  private

  # If there is a path in the graph from requires back to role_id,
  # then allowing this RoleRequire to be created would make the role graph
  # cyclic, and that is Not Allowed.
  def enforce_acyclic
    us = role_id
    them = Role.find_by(name: preceeds)
    # If our source role has not been added yet, then there is no way we can
    # tell if adding this RoleRequires will result in a cyclic graph.
    # However, in that case the cycle will be detected when the role that
    # requires refers to adds its own RoleRequires, so we don't have to worry
    # about it here in any case.
    return true if them.nil?
    # Find all the roles that target_role depends on that depend on
    # source_role. If there are none, then adding this role cannot create
    # a dependency loop, and we are OK.
    return true if Role.where("id in (
                                  select role_id from all_role_requires
                                  where role_id = ? AND required_role_name = ?)",
                              us,
                              them.name).empty?
    # Well, that is that. We will die, but we will be precise about why we die.
    errors[:base] << "RolePreceed: #{role.name} preceeding #{them.name} makes the role graph cyclic!"
    paths = Role.connection.select_all("select path from all_role_requires_paths where child_name = '#{them.name}' AND parent_name = '#{role.name}'")
    unless paths.empty?
      errors[:base] << "Role dependency chains that would be made cyclic:"
      paths.rows.each do |path|
        errors[:base] << "  #{path[0][1..-2].split(",").join(" -> ")}  (-> #{preceeds})"
      end
    end
  end

  def resolve_preceeds
    resolve!
  end

end
