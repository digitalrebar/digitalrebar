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

class BarclampProvisioner::OsInstall < Role

  def on_todo(nr)
    NodeRole.transaction do
      node = nr.node
      return if (["local"].member? node.bootenv) || (nr.run_count > 0)
      target = Attrib.get("provisioner-target_os",nr)
      Rails.logger.info("provisioner-install: Trying to install #{target} on #{node.name} (bootenv: #{node.bootenv})")
      node.bootenv = "#{target}-install"
      node.save!
    end
  end

  def on_active(nr)
    NodeRole.transaction do
      node = nr.node
      return if ["local"].member? node.bootenv || (nr.run_count > 1)
      node.power[:reset] ? node.power.reset : node.power.reboot
    end
  end

end
