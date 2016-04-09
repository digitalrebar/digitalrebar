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

class GoogleProvider < CloudProvider

  before_save :inject_google

  def self.template
    { 
    	url: {
    		type: "img",
 	   		src: "https://cloud.google.com/_static/8fea0d66ce/images/new-gcp-logo.png",
        href: "https://cloud.google.com",
        title: "Google Compute Engine"
  		},
    	google_project: {
    		type: "text",
    		default: "",
    		length: 30,
    		name: I18n.t('project', scope: "providers.show.google" )
  		},
    	google_json_key: {
    		type: "json_key",
    		default: "",
    		length: 30,
    		name: I18n.t('json_key', scope: "providers.show.google" )
  		}
    }
  end

  private

  def inject_google
    auth_details['provider'] = 'Google'
  end

end
