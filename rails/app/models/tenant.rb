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

require 'json'

class Tenant < ActiveRecord::Base

  after_create      :load_uuid

  def load_uuid
    self.reload
  end

  private :load_uuid

  belongs_to      :parent,       class_name: "Tenant"
  has_many        :users,        through: :user_tenant_capabilities 
  has_many        :capabilities, through: :user_tenant_capabilities 

end

