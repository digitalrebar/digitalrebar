#
# Cookbook Name:: rebar-bootstrap
# Recipe:: gemstuff
#
# Copyright (C) 2014 Dell, Inc.
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
#

tftproot = node["bootstrap"]["tftproot"]
tracedir=node["bootstrap"]["tracedir"]

directory tracedir do
  recursive true
end

node[:bootstrap][:gems].each do |gem|
  bash "install gem #{gem}" do
    code "gem install #{gem}"
    not_if "gem list --local |grep -q '^#{gem}'"
  end
end

directory "#{tftproot}/gemsite/gems" do
  action :create
  recursive true
end

bash "Create skeleton local gemsite" do
  cwd "#{tftproot}/gemsite"
  code "gem generate_index"
  not_if "test -d '#{tftproot}/gemsite/quick'"
end

["/var/run/rebar",
 "/var/cache/rebar",
 "/var/cache/rebar/cookbooks",
 "/var/cache/rebar/gems",
 "/var/cache/rebar/bin",
 "/var/log/rebar"
].each do |d|
  directory d do
    owner "rebar"
    action :create
    recursive true
  end
end

# warning for common error
if File.exists?("/opt/digitalrebar/core/rails/Gemfile.lock")
  Chef::Log.info("Using existing Gemfile.lock.  This will cause errors if Gemfile has been updated.  Delete lock and retry")
end 

bash "install required gems for Rebar using Bundler" do
  code "su -l -c 'cd /opt/digitalrebar/core/rails; bundle install --path /var/cache/rebar/gems --standalone --binstubs /var/cache/rebar/bin' rebar"
end
