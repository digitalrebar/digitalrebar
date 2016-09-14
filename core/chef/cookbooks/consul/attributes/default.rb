#
# Copyright 2014 John Bellone <jbellone@bloomberg.net>
# Copyright 2014 Bloomberg Finance L.P.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

default[:consul][:base_url] = 'https://releases.hashicorp.com/consul'
default[:consul][:version] = '0.6.3'
default[:consul][:install_dir] = '/usr/local/bin'
default[:consul][:checksums] = {
  '0.6.3_darwin_386'   => '7fb30756504cd9559c9b23e5d0d8d73a847ee62ed85d39955b5906c2f59a5bc1',
  '0.6.3_darwin_amd64' => '6dff4ffc61d66aacd627a176737b8725624718a9e68cc81460a3df9b241c7932',
  '0.6.3_linux_386'    => '2afb65383ab913344daaa9af827c1e8576c7cae16e93798048122929b6e4cc92',
  '0.6.3_linux_amd64'  => 'b0532c61fec4a4f6d130c893fd8954ec007a6ad93effbe283a39224ed237e250',
  '0.6.3_web_ui'       => '93bbb300cacfe8de90fb3bd5ede7d37ae6ce014898edc520b9c96a676b2bbb72'
}


# Service attributes
default[:consul][:service_mode] = 'client'
default[:consul][:data_dir] = '/var/lib/consul'
default[:consul][:config_dir] = '/etc/consul.d'
default[:consul][:servers] = []

# UI attributes
default[:consul][:client_addr] = '0.0.0.0'
default[:consul][:ui_dir] = '/var/lib/consul/ui'
default[:consul][:serve_ui] = true
default[:consul][:service_group] = 'consul'
