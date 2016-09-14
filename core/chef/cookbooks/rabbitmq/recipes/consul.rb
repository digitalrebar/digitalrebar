#
# Cookbook Name:: rabbitmq
# Recipe:: consul
#
# Author: Greg Althaus <galthaus@austin.rr.com>
# Copyright (C) 2015 Greg Althaus
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
require 'json'

include_recipe 'rabbitmq::default'

bash "reload consul rabbitmq" do
  code "/usr/local/bin/consul reload"
  action :nothing
end

ip_addr = (IP.coerce(node['rabbitmq']['service_address']).addr rescue nil)
port = node['rabbitmq']['port'] || 5672

template '/etc/consul.d/rebar-rabbitmq.json' do
  source 'consul-rabbitmq-server.json.erb'
  mode 0644
  owner 'root'
  variables(:ip_addr => ip_addr, :port => port)
  notifies :run, 'bash[reload consul rabbitmq]', :immediately
end
