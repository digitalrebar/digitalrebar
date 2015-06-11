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
default[:consul][:version] = '0.5.2'
default[:consul][:install_dir] = '/usr/local/bin'
default[:consul][:checksums] = {
  '0.5.0_darwin_amd64' => '24d9758c873e9124e0ce266f118078f87ba8d8363ab16c2e59a3cd197b77e964',
  '0.5.0_linux_386'    => '4b6147c30596a30361d4753d409f8a1af9518f54f5ed473a4c4ac973738ac0fd',
  '0.5.0_linux_amd64'  => '161f2a8803e31550bd92a00e95a3a517aa949714c19d3124c46e56cfdc97b088',
  '0.5.0_web_ui'       => '0081d08be9c0b1172939e92af5a7cf9ba4f90e54fae24a353299503b24bb8be9',
  '0.5.2_darwin_amd64' => '87be515d7dbab760a61a359626a734f738d46ece367f68422b7dec9197d9eeea',
  '0.5.2_linux_386'    => '29306ce398109f954ceeea3af79878be4fb0d949f8af3a27c95ccef2101e8f60',
  '0.5.2_linux_amd64'  => '171cf4074bfca3b1e46112105738985783f19c47f4408377241b868affa9d445',
  '0.5.2_web_ui'       => 'ad883aa52e1c0136ab1492bbcedad1210235f26d59719fb6de3ef6464f1ff3b1'
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
