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

default[:consul][:base_url] = 'http://dl.bintray.com/mitchellh/consul/'
default[:consul][:version] = '0.5.0'
default[:consul][:install_dir] = '/usr/local/bin'
default[:consul][:checksums] = {
  '0.4.1_darwin_amd64' => '957fe9ba27bbaf99539cd534db8ac8ec4c9fa1c6b3b4675d0c0eb3a7fbfb646c',
  '0.4.1_linux_386'    => 'a496d6fd8ff5b460aea50be5d20fbd95cb5d30e9018259a0540273a17fae1c25',
  '0.4.1_linux_amd64'  => '2cf6e59edf348c3094c721eb77436e8c789afa2c35e6e3123a804edfeb1744ac',
  '0.4.1_web_ui'       => 'e02929ed44f5392cadd5513bdc60b7ab7363d1670d59e64d2422123229962fa0',
  '0.4.1_windows_386'  => '61906f5d73a0d991dae5d75a25299f183670efa473cd155c715eefc98ce49cc8',
  '0.5.0_darwin_amd64' => '24d9758c873e9124e0ce266f118078f87ba8d8363ab16c2e59a3cd197b77e964',
  '0.5.0_linux_386'    => '4b6147c30596a30361d4753d409f8a1af9518f54f5ed473a4c4ac973738ac0fd',
  '0.5.0_linux_amd64'  => '161f2a8803e31550bd92a00e95a3a517aa949714c19d3124c46e56cfdc97b088',
  '0.5.0_web_ui'       => '0081d08be9c0b1172939e92af5a7cf9ba4f90e54fae24a353299503b24bb8be9'
}


# Service attributes
default[:consul][:service_mode] = 'bootstrap'
default[:consul][:data_dir] = '/var/lib/consul'
default[:consul][:config_dir] = '/etc/consul.d'
default[:consul][:servers] = []

# UI attributes
default[:consul][:client_addr] = '0.0.0.0'
default[:consul][:ui_dir] = '/var/lib/consul/ui'
default[:consul][:serve_ui] = true
