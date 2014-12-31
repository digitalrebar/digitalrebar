#
# Cookbook Name:: crowbar-bootstrap
# Recipe:: sledgehammer
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

directory tracedir do
  recursive true
end

sledgehammer_signature = node["bootstrap"]["sledgehammer"]["signature"]

sledgehammer_url="#{node["bootstrap"]["sledgehammer"]["url"]}/#{sledgehammer_signature}"
sledgehammer_dir="#{tftproot}/sledgehammer/#{sledgehammer_signature}"

directory sledgehammer_dir do
  action :create
  recursive true
end

ruby_block "Download Sledgehammer #{sledgehammer_signature}" do
  block do
    files = ["initrd0.img","vmlinuz0","sha1sums"]
    Dir.chdir(sledgehammer_dir) do |path|
      unless File.file?("sha1sums")
        files.each do |f|
          dload = "#{sledgehammer_url}/#{f}"
          Chef::Log.info("Downloading #{dload}")
          raise("Cannot download #{dload}") unless system("curl -L -f -O '#{dload}'")
        end
      end
      raise("Sledgehammer image at #{sledgehammer_dir} corrupt or nonexistent") unless system("sha1sum -c sha1sums")
    end
  end
end

directory "#{tftproot}/discovery" do
  action :create
  recursive true
end

["initrd0.img", "vmlinuz0"].each do |f|
  link "#{tftproot}/discovery/#{f}" do
    action :create
    link_type :symbolic
    to "../sledgehammer/#{sledgehammer_signature}/#{f}"
  end
end
