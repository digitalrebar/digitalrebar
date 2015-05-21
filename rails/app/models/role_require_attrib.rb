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

class RoleRequireAttrib < ActiveRecord::Base

  audited

  belongs_to      :role
  has_one         :attrib,      :class_name => "Attrib", :foreign_key => "name", :primary_key => "attrib_name"

  def attrib_at
    aat = read_attribute("attrib_at") || attrib.map
    return aat if aat
    raise("RoleRequireAttrib: Cannot find where to put attrib data for #{attrib_name}")
  end

  def map(value)
    keys = attrib_at.split('/')
    raise "Cannot deal with an empty map!" if keys.empty?
    res = value
    while !keys.empty? do
      res = {keys.pop => res}
    end
    res
  end
end
