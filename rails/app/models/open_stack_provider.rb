# Copyright 2016 RackN
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

class OpenStackProvider < CloudProvider

  before_save :inject_packet

  def self.template
    { 
      url: {
        type: "img",
        src: "https://www.openstack.org/assets/openstack-logo/R/openstack-cloud-software-vertical-web.png"
      },
  	  :'os_auth_url' => {
        type: "text",
        default: "",
        length: 50,
        name: I18n.t('os-auth-url', scope: "providers.show.openstack" )
      },
      :'os-username' => {
        type: "text",
        default: "",
        length: 30,
        name: I18n.t('os-username', scope: "providers.show.openstack" )
      },
      :"os-password" => {
        type: "password",
        default: "",
        length: 30,
        name: I18n.t('os-password', scope: "providers.show.openstack" )
      },
      :"os-project-name" => {
        type: "text",
        default: "",
        length: 30,
        name: I18n.t('os-project-name', scope: "providers.show.openstack" )
      },
      :"os-region-name" => {
        type: "text",
        default: "",
        length: 30,
        name: I18n.t('os-region-name', scope: "providers.show.openstack" )
      }
    }
  end

  private

  def inject_packet
    auth_details['provider'] = 'OpenStack'
  end

end
