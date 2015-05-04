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

include_recipe "bios::bios-common"

provisioner_server = node[:crowbar][:provisioner][:server][:webservers].first[:url]

include_recipe "bios::bios-tools"

problem_file = "/var/log/chef/hw-problem.log"
product = node[:dmi][:system][:product_name]
product.strip!
%w{bios bmc}.each do |t|
  next unless (node["bios"]["updaters"][product][t] rescue nil)
  f = node["bios"]["updaters"][product][t]
  if f.include?('/')
    directory "/tmp/#{f.split('/')[0..-2].join('/')}" do
      recursive true
    end
  end
  remote_file "/tmp/#{f}" do
    source "#{provisioner_server}/files/bios/#{f}"
    mode '0755'
    action :create_if_missing
  end
end

bios_update "bmc" do
  type            "bmc"
  problem_file    problem_file
  product         product
  max_tries       node[:bios][:max_tries]
  only_if         { @@bmc_update_enable }
  action   :update
end


bios_update "bios" do
  type            "bios"
  problem_file    problem_file
  product         product
  max_tries       node[:bios][:max_tries]
  only_if         { @@bios_update_enable }
  action   :update
end

bios_update "wsman" do
  type           "wsman"
  problem_file    problem_file
  product         product
  max_tries       node[:bios][:max_tries]
  only_if         { @@bios_update_enable }
  action   :update
end
