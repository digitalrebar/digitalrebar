# Copyright 2014 Benny Wong <benny@bwong.net>
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

install_version = [node[:consul][:version], 'web_ui'].join('_')
install_checksum = node[:consul][:checksums].fetch(install_version)

package "unzip"

bash "Fetch consul UI for #{install_version}" do
  code <<EOC
set -e
mkdir -p "#{node[:consul][:ui_dir]}"
cd "#{node[:consul][:ui_dir]}"
curl -f -L -O '#{node[:consul][:base_url]}/#{install_version}.zip'
echo '#{install_checksum}  #{install_version}.zip' > sha256sums
sha256sum -c --status sha256sums
unzip "#{install_version}.zip"
mv dist/* .
rm -rf dist
EOC
  not_if { ::File.exists?("#{node[:consul][:ui_dir]}/static") }
end
