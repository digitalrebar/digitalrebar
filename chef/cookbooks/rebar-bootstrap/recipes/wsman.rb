#
# Cookbook Name:: rebar-bootstrap
# Recipe:: wsman
#
# Copyright (C) 2014 Victor Lowther
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
# Build wsman for the ipmi barclamp
# It would be great if the bootstrap chef recipes were composable...
# Also note that this is pure evil in a can, and should really
# be wrapped up in an RPM if we ever want to use it outside of Docker.

return unless /^(redhat|centos)-7/ =~ "#{node[:platform]}-#{node[:platform_version]}"

tracedir=node["bootstrap"]["tracedir"]
directory tracedir do
  recursive true
end

wsman_repo=node["bootstrap"]["openwsman"]["repo"]
wsman_ver=node["bootstrap"]["openwsman"]["version"]

bash "clone openwsman from #{wsman_repo}" do
  cwd "/root"
  code "git clone https://github.com/openwsman/openwsman"
  not_if "test -d /root/openwsman/.git"
  not_if "fgrep -q '#{wsman_ver}' '#{tracedir}/openwsman'"
end

bash "Check out openwsman version #{wsman_ver}" do
  cwd "/root/openwsman"
  code "git clean -f -x -d; git checkout -f '#{wsman_ver}'"
  not_if "git status 2>&1 |fgrep -q '#{wsman_ver}'"
  not_if "fgrep -q '#{wsman_ver}' '#{tracedir}/openwsman'"
end

bash "Build openwsman #{wsman_ver}" do
  cwd "/root/openwsman"
  code <<EOC
set -e
mkdir build
cd build
cmake -D BUILD_PYTHON=NO \
      -D BUILD_JAVA=NO \
      -D BUILD_EXAMPLES=NO \
      -D BUILD_PERL=NO \
      -D DISABLE_SERVER=YES \
      -D USE_PAM=NO \
      -D BUILD_RUBY=YES \
      -D BUILD_RUBY_GEM=YES \
      -D CMAKE_INSTALL_PREFIX=/usr \
      ..
make
make install
echo '#{wsman_ver}' > '#{tracedir}/openwsman'
rm -rf /root/openwsman"
EOC
  not_if "fgrep -q '#{wsman_ver}' '#{tracedir}/openwsman'"
end
