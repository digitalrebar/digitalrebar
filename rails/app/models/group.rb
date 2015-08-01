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

class Group < ActiveRecord::Base
  
  CATEGORIES = %w(ui rack tag)

  validates_format_of :name, :with=>/\A[a-zA-Z][_a-zA-Z0-9]*\z/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")
  validates_format_of :category, :with=>/\A[a-zA-Z][_a-zA-Z0-9]*\z/, :message => I18n.t("db.lettersnumbers", :default=>"Category limited to [_a-zA-Z0-9]")

  validates_uniqueness_of :name, :scope => :category, :case_sensitive => false, :message => I18n.t("db.notuniqueincategory", :default=>"Name item must be unique within category")
  validates_inclusion_of :category, :in => CATEGORIES, :message => I18n.t("db.group_category", :default=>"Illegal group category")
  # REMINDER: if you add a new group category, update the g(categories) list in BDD/groups.erl!

  has_and_belongs_to_many :nodes, :join_table => "node_groups", :foreign_key => "group_id"

  def <=>(other)
    # use Array#<=> to compare the attributes
    [self.order, self.name] <=> [other.order, other.name]
  end

  def to_s
    name
  end

end

