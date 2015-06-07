#
# Cookbook Name:: crowbar-bootstrap
# Recipe:: consul
#
# Copyright (C) 2014 Greg Althaus
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

require "securerandom"

if File.exists?("/etc/consul.d/default.json")
  Chef::Log.info("Read consul config from /etc/consul.d/default.json")
  node.normal[:consul] = JSON.parse(File.read("/etc/consul.d/default.json"))
else
  Chef::Log.info("Creating new Consul config")
  node.normal[:consul] = { :serve_ui => true,
                        :datacenter => "opencrowbar",
                        :acl_datacenter => "opencrowbar",
                        :acl_master_token => SecureRandom.uuid,
                        :encrypt => SecureRandom.base64,
                        :disable_remote_exec => true,
                        :acl_default_policy => "allow",
                        :acl_down_policy => "allow"
                      }
end
Chef::Log.info("Consul config: #{node[:consul].inspect}")
