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
require 'test_helper'
 
class GroupModelTest < ActiveSupport::TestCase
  
  test "Naming Conventions" do
    assert_raise(ActiveRecord::RecordInvalid) { Group.create!(:name=>"1123") }
    assert_raise(ActiveRecord::RecordInvalid) { Group.create!(:name=>"1foo") }
    assert_raise(ActiveRecord::RecordInvalid) { Group.create!(:name=>"Ille!gal") }
    assert_raise(ActiveRecord::RecordInvalid) { Group.create!(:name=>" nospaces") }
    assert_raise(ActiveRecord::RecordInvalid) { Group.create!(:name=>"no spaces") }
    assert_raise(ActiveRecord::RecordInvalid) { Group.create!(:name=>"nospacesatall ") }
  end

  test "Unique Name no dup name+category" do
    g = Group.create! :name=>"foo", :category=>'rack'
    assert_not_nil g, "Node Created"
    assert_raise(ActiveRecord::RecordInvalid, ActiveRecord::RecordNotUnique) { Group.create!(:name=>"foo", :category=>'rack') }
  end

  test "default category" do
    g = Group.create!(:name=>"foo")
    assert_not_nil g, "Node Created"
    assert_equal g.category, 'ui', "default category"
  end
  
  test "alternate category" do
    g = Group.create!(:name=>"foo", :category=>'rack')
    assert_not_nil g, "Node created"
    assert_equal g.name, 'foo', "name right"
    assert_equal g.category, 'rack', "category right"
  end
  
  test "only allowed categories" do
    assert_raise(ActiveRecord::RecordInvalid) { Group.create!(:name=>"foo", :category=>'foo') }
  end


end

