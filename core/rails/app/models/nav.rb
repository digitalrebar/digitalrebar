# Copyright 2012, Dell
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
#
class Nav < ActiveRecord::Base
  
  self.primary_key = "item"  
  
  belongs_to :parent, :class_name => "Nav", :foreign_key => "parent_item"
  has_many :children, :class_name => "Nav", :foreign_key => "parent_item"
  
  validates_uniqueness_of :item, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Name item must be unique")
  
  scope   :item,    ->(item) { where(['item=?', item]) }

end
