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

### at some point we'll support raid on other platforms... for now just RHEL derived.
return unless ["centos","redhat"].member?(node[:platform])
raid_utils = "#{node[:crowbar][:provisioner][:server][:webserver]}/files/raid"

[{ executable: "/usr/sbin/sas2ircu",
   archive: 'SAS2IRCU_P19.zip',
   source: "http://www.lsi.com/downloads/Public/Host%20Bus%20Adapters/Host%20Bus%20Adapters%20Common%20Files/SAS_SATA_6G_P19/SAS2IRCU_P19.zip",
   installcode: 'cd /usr/sbin && unzip -j -o "/tmp/SAS2IRCU_P19.zip" "SAS2IRCU_P19/sas2ircu_linux_x86_rel/sas2ircu" && chmod 755 sas2ircu' },
 { executable: '/opt/MegaRAID/MegaCli/MegaCli64',
   archive: '8.07.14_MegaCLI.zip',
   source: 'http://www.lsi.com/downloads/Public/RAID%20Controllers/RAID%20Controllers%20Common%20Files/8.07.14_MegaCLI.zip',
   installcode: 'for pkg in "Linux/MegaCli-8.07.14-1.noarch.rpm"; do unzip -j -o 8.07.14_MegaCLI.zip "$pkg" && rpm -Uvh "${pkg##*/}"; done'
 }].each do |util|
  bash "Fetch #{util[:archive]}" do
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
    code util[:installcode]
    not_if { File.executable?(util[:executable]) }
  end
end

raid_config "lsi-raid-config" do
  config nil
  debug_flag node[:raid][:debug]
  nic_first true
  action [ :report ]
end
