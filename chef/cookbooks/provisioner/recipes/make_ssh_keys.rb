# Copyright 2011, Dell
# Copyright 2015, RackN
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

# Build my key
unless ::File.exists?("/root/.ssh/id_rsa.pub")
  %x{ssh-keygen -t rsa -f /root/.ssh/id_rsa -N ""}
end
node.normal["rebar"]["root_public_key"] = IO.read("/root/.ssh/id_rsa.pub").strip

