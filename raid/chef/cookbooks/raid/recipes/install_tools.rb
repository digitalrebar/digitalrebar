# Copyright (c) 2013 Dell Inc.
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

raid_utils = "#{node[:crowbar][:provisioner][:server][:webserver]}/files/raid"

%w{unzip rpm2cpio curl}.each do |pkg|
  next if system("which #{pkg}")
  package pkg
end

node["raid"]["drivers"].each do |util|
  bash "Fetch #{util[:archive]} for #{util[:name]}" do
    cwd '/tmp'
    code <<EOC
curl -f -O '#{raid_utils}/#{util[:archive]}' && exit 0
echo '#{util[:archive]} not present at #{raid_utils}'
echo
echo 'Please download it from #{util[:source]}'
exit 1
EOC
    not_if { File.file?("/tmp/#{util[:archive]}") }
  end

  bash "Install #{util[:executable]}" do
    cwd '/tmp'
    code util[:linux_installcode]
    not_if { File.executable?(util[:executable]) }
  end
end
