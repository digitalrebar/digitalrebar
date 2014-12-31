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
 
class I18nTest < ActiveSupport::TestCase

  # ensure that the localization files are not corrupted!  
  test "Ensure i18n works" do
    s = I18n.t('nav.help')
    assert_equal s, 'Help'
  end

  test "i18n default work" do
    assert_equal I18n.t('this_should_not_match_anything', :default=>'foo'), 'foo'
  end

  test "Chuck Norris" do
    assert_equal I18n.t('chuck_norris'), 'Die!!!'
  end

end