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

class AwsProvider < CloudProvider

  before_save :inject_aws

  after_create      :load_uuid

  def load_uuid
    self.reload
  end

  private :load_uuid

  def self.template
    { 
    	url: {
    		type: "img",
 	   		src: "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1d/AmazonWebservices_Logo.svg/2000px-AmazonWebservices_Logo.svg.png",
        href: "https://aws.amazon.com",
        title: "Amazon EC2"
  		},
    	aws_access_key_id: {
    		type: "text",
    		default: "",
    		length: 30,
    		name: I18n.t('access_key_id', scope: "providers.show.aws" )
  		},
    	aws_secret_access_key: {
    		type: "password",
    		default: "",
    		length: 30,
    		name: I18n.t('secret_access_key', scope: "providers.show.aws" )
  		},
    	region: {
    		type: "text",
    		default: "us-west-2",
    		length: 30,
    		name: I18n.t('region', scope: "providers.show.aws" )
  		},
      vpc_id: {
        type: "text",
        default: "!default",
        length: 30,
        name: I18n.t('vpc_id', scope: "providers.show.aws" )
      },
      'provider-create-hint' => {
        type: "json_key",
        default: {
          image_id: "default ami"
        }
      }
    }
  end

  private

  def inject_aws
    auth_details['provider'] = 'AWS'
  end
  
end
