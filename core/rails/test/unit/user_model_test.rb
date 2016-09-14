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
require 'digest/md5'
 
class UserModelTest < ActiveSupport::TestCase

  test "Realm did not change" do
    assert_equal User::DIGEST_REALM, "Rebar"
  end

  test "Verify Password Works" do
    user = "thisisatestuser"    # must be LOWER CASE
    password = "1UnguessablePassword!"

    u = User.create!(:username=>user, :password=>password, :password_confirmation=>password)
    assert_equal password, u.password
    
    # unless we get a new one, the cache object will trick us!
    u2 = User.find_by_username user
    assert_equal user, u2.username
    assert_not_equal password, u2.password
    assert_equal true, u2.valid_password?(password)
  end
  
  test "Verify Digest" do
    user = "thisisatestuser"    # must be LOWER CASE
    password = "1UnguessablePassword!"
    digest = (Digest::MD5.hexdigest([user,User::DIGEST_REALM,password].join(":")))

    u = User.create!(:username=>user, :password=>password, :password_confirmation=>password)
    assert_equal password, u.password
    
    # unless we get a new one, the cache object will trick us!
    u2 = User.find_by_username user
    assert_equal user, u2.username
    assert_not_equal password, u2.password
    assert_not_equal digest, u2.encrypted_password
    assert_equal true, u2.valid_password?(password)
    u2_pass = u2.encrypted_password
    
    u.digest_password(password)
    u.save!
    
    u3 = User.find_by_username user
    assert_equal user, u3.username
    assert_not_equal u2_pass, u3.encrypted_password
    assert_not_equal password, u3.password
    assert_equal digest, u3.encrypted_password
    assert_equal true, u3.valid_password?(password)
  end
  
  test "verify Unique" do
    user = "ThisIsATestUser"
    password = "UnguessablePassword!"
    u = User.create!(:username=>user, :password=>password)
    assert_raise(ActiveRecord::RecordInvalid, ActiveRecord::RecordNotUnique) { User.create!(:username=>user, :password=>password) }
  end

end

