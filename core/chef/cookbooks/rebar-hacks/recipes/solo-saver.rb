# Copyright 2013, Dell
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

# This recipe is only for use by chef-solo.  It saves all the attributes
# on a node object to a known location on the filesystem.

ruby_block "Save attributes at the end of run" do
  block do
    File.open("/var/chef/node-out.json","w") do |f|
      f.write(JSON.pretty_generate(node.for_json))
    end
  end
end
