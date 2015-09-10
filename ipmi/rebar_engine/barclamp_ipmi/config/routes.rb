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

BarclampIpmi::Engine.routes.draw do

  # API routes
  scope :defaults => {:format=> 'json'} do
    constraints( :api_version => /v[1-9]/ ) do
      scope ':api_version' do

        resources :barclamps do
          collection do
            get :catalog
          end
          member do
      
          end
        end
      end
    end
  end

end
