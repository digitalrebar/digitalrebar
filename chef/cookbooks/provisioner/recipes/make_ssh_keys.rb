# Copyright 2011, Dell
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
directory "/root/.ssh" do
  owner "root"
  group "root"
  mode "0700"
  action :create
end

node.normal["crowbar"]["provisioner"]["server"]["access_keys"] ||= Mash.new
if node["crowbar"]["provisioner"]["server"]["access_keys"].empty? &&
    File.exists?("/root/.ssh/authorized_keys")
  count=1
  IO.foreach("/root/.ssh/authorized_keys") do |line|
    node.normal["crowbar"]["provisioner"]["server"]["access_keys"]["admin-#{count}"] = line.strip
    count += 1
  end
end


# Build my key
unless ::File.exists?("/root/.ssh/id_rsa.pub")
  %x{ssh-keygen -t rsa -f /root/.ssh/id_rsa -N ""}
  node.normal["crowbar"]["provisioner"]["server"]["access_keys"][node["fqdn"]] = IO.read("/root/.ssh/id_rsa.pub").strip
end
