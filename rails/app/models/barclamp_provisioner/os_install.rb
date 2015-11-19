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
    node = nr.node
    return if (["local"].member? node.bootenv) || (nr.run_count > 0)
    node.with_lock('FOR NO KEY UPDATE') do
      disks = Attrib.get('disks',node) || []
      claims = Attrib.get('claimed-disks',node) || {}
      target_disk = Attrib.get('operating-system-disk',node)
      unless target_disk &&
             claims[target_disk] == 'operating system' &&
             disks.any?{|d|d['unique_name'] == target_disk}
        target_disk = nil
        claims.delete_if{|k,v|v == 'operating system'}
        # Grab the first unclaimed nonremovable disk for the OS.
        target_disk = disks.detect{|d|!d["removable"] && !claims[d['unique_name']]}
        Attrib.set('operating-system-disk',node,target_disk["unique_name"])
        claims[target_disk["unique_name"]] = "operating system"
        Attrib.set('claimed-disks',node,claims)
      end
      # Extract repos that the provisioner provides out and add them to this node if needed.
      provisioner_node_id = Attrib.get('provisioner-node-id',node)
      if provisioner_node_id > 0
        provisioner_node = Node.find_by!(id: provisioner_node_id)
        provisioner_repos = Attrib.get('provisioner-provided-repos',provisioner_node)
        Rails.logger.info("base_images: #{provisioner_repos.inspect}")
        if provisioner_repos
          repos_to_add={}
          provisioner_repos.each do |os,repos|
            repos.each do |name,urls|
              repos_to_add[name] ||= {
                'name' => "provisioner-#{name}",
                'description' => "Repositories provided by the provisioner in #{nr.deployment.name}",
                'disabled' => false,
                'oses' => []
              }
              repos_to_add[name]['oses'] << {'os' => os, 'repos' => urls}
            end
          end
          Rails.logger.info("base_images: #{repos_to_add.inspect}")
          old_repos = Attrib.get('package-repositories',node)
          new_repos = old_repos.
                      reject{|e|repos_to_add.keys.member?(e['name'])}.
                      concat(repos_to_add.values)
          Attrib.set('package-repositories',node,new_repos) unless old_repos == new_repos
        end
      end
      target_os = Attrib.get("provisioner-target_os",nr)
      Rails.logger.info("provisioner-install: Trying to install #{target_os} on #{node.name} (bootenv: #{node.bootenv})")
      node.bootenv = "#{target_os}-install"
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
