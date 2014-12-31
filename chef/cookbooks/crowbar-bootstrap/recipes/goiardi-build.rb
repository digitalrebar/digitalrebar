#
# Cookbook Name:: crowbar-bootstrap
# Recipe:: goiardi-build
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

ENV["GOPATH"]=node["bootstrap"]["gopath"]
bash "Install sqitch for database management" do
  code <<EOC
curl -L http://cpanmin.us | perl - --sudo App::cpanminus
cpanm --quiet --notest App::Sqitch
EOC
  not_if "which sqitch"
end

goiardi_repo=node["bootstrap"]["goiardi"]["repo"]

goiardi_src="#{ENV["GOPATH"]}/src/#{goiardi_repo}"

bash "Fetch Goiardi Source" do
  code "/usr/local/go/bin/go get -t '#{goiardi_repo}'"
  not_if { File.directory?(goiardi_src) }
end

bash "Build and Install Goiardi" do
  cwd goiardi_src
  code "/usr/local/go/bin/go build && mv goiardi /usr/local/bin"
  not_if "which goiardi"
end

%w{/etc/goiardi /var/cache/goiardi}.each do |d|
  directory d do
    action :create
    recursive true
  end
end

template "/etc/systemd/system/goiardi.service" do
  only_if "test -d /etc/systemd/system"
  source "goiardi.service.erb"
end
