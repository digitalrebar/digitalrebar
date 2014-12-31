# Copyright 2013, Dell 
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
 
class NodeModelTest < ActiveSupport::TestCase

  def setup
    @crowbar = Barclamp.find_by_name("crowbar") 
    assert_not_nil @crowbar
    d = Deployment.find_or_create_by(name: 'system', description: 'automatic')
    assert_not_nil d, 'we need at least 1 Deployment'
    #  We need a system deployment to create default proposals in.
    d.send(:write_attribute,"system",true)
    d.save!
  end

  test "name too long" do
    assert_raise(ActiveRecord::RecordInvalid) { Node.create!(:name=>"12345678901234567890123456789012345678901234567890.12345678901234567890123456789012345678901234567890.12345678901234567890123456789012345678901234567890.12345678901234567890123456789012345678901234567890.12345678901234567890123456789012345678901234567890.com") }
  end
  
  test "Naming Conventions" do
    assert_raise(ActiveRecord::RecordInvalid) { Node.create!(:name=>"fqdnrequired") }
    assert_raise(ActiveRecord::RecordInvalid) { Node.create!(:name=>"Ille!gal.foo.org") }
    assert_raise(ActiveRecord::RecordInvalid) { Node.create!(:name=>" nospaces.bar.it") }
    assert_raise(ActiveRecord::RecordInvalid) { Node.create!(:name=>"no spaces.dell.com") }
    assert_raise(ActiveRecord::RecordInvalid) { Node.create!(:name=>"nospacesatall.end.edu ") }
    assert_raise(ActiveRecord::RecordInvalid) { Node.create!(:name=>"musthaveatleastthreedomains.com") }
  end

end

