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

class RebarForbiddenError < RebarError
  attr_accessor :rebar_model, :rebar_column, :rebar_key

  def initialize(key, model, column = nil)
    @rebar_model = model
    @rebar_key = key
    @rebar_column = column
  end

end
