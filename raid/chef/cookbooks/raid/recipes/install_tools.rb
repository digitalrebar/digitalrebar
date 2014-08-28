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
include_recipe "utils"

return unless ["centos","redhat"].member?(node[:platform]) && !@@is_admin

provisioners = search(:node, "roles:provisioner-server")
provisioner = provisioners[0] if provisioners
web_port = provisioner["provisioner"]["web_port"]
address = Chef::Recipe::Barclamp::Inventory.get_network_by_type(provisioner, "admin").address
provisioner_server = "#{address}:#{web_port}"
log("Provisioner server info is #{provisioner_server}")

return unless provisioner_server

sas2ircu="SAS2IRCU_P16.zip"
megacli="8.07.07_MegaCLI.zip"

[sas2ircu,megacli].each do |f|
  remote_file "/tmp/#{f}" do
    source "http://#{provisioner_server}/files/raid/tools/#{f}"
    action :create_if_missing
  end
end

bash "install sas2ircu" do
  code <<EOC
cd /usr/sbin
[[ -x /usr/sbin/sas2ircu ]] && exit 0
unzip -j -o "/tmp/#{sas2ircu}" "SAS2IRCU_P16/sas2ircu_linux_x86_rel/sas2ircu"
chmod 755 sas2ircu
EOC
end

bash "install megacli" do
  code <<EOC
cd /tmp 
[[ -x /opt/MegaRAID/MegaCli/MegaCli64 ]] && exit 0
for pkg in "linux/MegaCli-8.07.07-1.noarch.rpm"; do
    unzip -j -o "#{megacli}" "$pkg"
  rpm -Uvh "${pkg##*/}"
done
EOC
end
