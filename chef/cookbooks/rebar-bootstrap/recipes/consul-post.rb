#
# Cookbook Name:: rebar-bootstrap
# Recipe:: consul-post
#
# Copyright (C) 2014 RackN, Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

# Make sure the rebar user has access to the things in the consul group
group "consul" do
  action :modify
  append true
  members "rebar"
end

# Create an digitalrebar private keyspace under digitalrebar/private
# One day we may want to require tokens for all, but ..
bash "Update anonymous ACL" do
  code <<EOC
echo '{
  "ID": "anonymous",
  "Type": "client",
  "Rules": "key \\"digitalrebar/private\\" { policy = \\"deny\\" }"
}' > /tmp/tmp_cred.json
EOF
curl -X PUT -d @/tmp/tmp_cred.json http://localhost:8500/v1/acl/update?token=#{node[:consul][:acl_master_token]}
rm -f /tmp/tmp_cred.json
EOC
end

  
