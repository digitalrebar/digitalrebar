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


action :add do
  tftproot = node["crowbar"]["provisioner"]["server"]["root"]
  new_resource.address.each do |mac|
    ["#{tftproot}/nodes/#{mac.downcase}.grub",
     "#{tftproot}/nodes/#{mac.upcase}.grub"].each do |grubfile|
      template grubfile do
        mode 0644
        owner "root"
        group "root"
        source "grub.cfg.erb"
        variables(:append_line => "crowbar.fqdn=#{new_resource.name} #{new_resource.kernel_params}",
                  :initrd => new_resource.initrd,
                  :machine_key => node["crowbar"]["provisioner"]["machine_key"],
                  :kernel => new_resource.kernel)
      end
    end
  end
end

action :remove do
  tftproot = node["crowbar"]["provisioner"]["server"]["root"]
  new_resource.address.each do |mac|
    grubfile = "#{tftproot}/nodes/#{mac.downcase}.grub"
    file grubfile do
      action :delete
    end
  end
end
