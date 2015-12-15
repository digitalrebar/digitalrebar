# Copyright 2015 RackN
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

class Provider < ActiveRecord::Base

  audited
  has_many :nodes

  def as_json(args = nil)
    super(args).merge("type" => self.type.to_s)
  end

  def create_node(obj)
    true
  end

  def reboot_node(obj)
    true
  end

  def delete_node(obj)
    true
  end

  def can_create_nodes
    false
  end

end
