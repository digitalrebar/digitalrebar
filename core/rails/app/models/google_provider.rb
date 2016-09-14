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

  after_create      :load_uuid

  def load_uuid
    self.reload
  end

  private :load_uuid
  
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
        default: '{ "replace_me": "with exact json from provider" }',
        length: 5,
    		name: I18n.t('json_key', scope: "providers.show.google" )
      },
      'provider-create-hint' => {
        type: "json_key",
        name: I18n.t('provider-create-hint', scope: "providers.show"),
        length: 15,
        default: {
          "disks" => [
            {
              "autoDelete" => true,
              "boot" => true,
              "type" => "PERSISTENT",
              "initializeParams" => {
                "sourceImage" => "projects/centos-cloud/global/images/centos-7-v20160803"
              }
            }
          ]
        }
      }
    }
  end

  def variant_default
    'google'
  end

  private

  def inject_google
    auth_details['provider'] = 'Google'
  end

end
