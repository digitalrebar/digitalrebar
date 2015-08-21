#
# Cookbook Name:: crowbar-bootstrap
# Recipe:: go
#
# Copyright (C) 2014 Victor Lowther.
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
ENV["GOPATH"]=node["bootstrap"]["gopath"]

directory node["bootstrap"]["gopath"] do
  action :create
end

goball = "go#{node["bootstrap"]["gover"]}.linux-amd64.tar.gz"
bash "Fetch and install Go" do
  code <<EOC
set -e
curl -fgL -o '/tmp/#{goball}' 'https://storage.googleapis.com/golang/#{goball}'
rm -rf /usr/local/go
tar -C '/usr/local' -xzf '/tmp/#{goball}'
rm '/tmp/#{goball}'
touch '#{tracedir}/go-#{node["bootstrap"]["gover"]}'
EOC
  not_if { File.directory?("/usr/local/go") && File.exists?("#{tracedir}/go-#{node["bootstrap"]["gover"]}") }
end

cookbook_file "/etc/profile.d/gopath.sh" do
  action :create
end

bash "Fetch and install the Crowbar CLI" do
  code <<EOC
. /etc/profile
go get -u github.com/VictorLowther/crowbar-api/crowbar
cp "$GOPATH/bin/crowbar" /usr/local/bin
EOC
  not_if "test -x /usr/local/go/bin/crowbar"
end
