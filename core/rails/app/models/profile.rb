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

class Profile < ActiveRecord::Base

  belongs_to :tenant

  validate  :check_sanity

  private

  def check_sanity
    errors.add(:values, "values must be a JSON object") unless values.is_a?(Hash)
    values.each do |k,v|
      attrib = Attrib.find_by(name: k)
      errors.add(:values, "No such attrib '#{k}', cannot be used in a profile") unless attrib
      if attrib
        errors.add(:values, "Attrib '#{k}' is not writable, cannot be set in a profile") unless attrib.writable
        begin
          attrib.kwalify_validate(v)
        rescue Attrib::AttribValidationFailed => e
          errors.add(:values, "provided value for '#{k}' fails schema validation: #{e.to_s}, cannot be used in a profile")
        end
      end
    end
  end

end

