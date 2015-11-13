# Copyright 2014, Victor Lowther
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

class BarclampProvisioner::BaseImages < Role

  def sysdata(nr)
    return {
      "rebar" => {
        "provisioner" => {
          "barclamps" => Barclamp.all.map{|bc|bc.cfg_data}
        }
      }
    }
  end

  def on_active(nr)
    # Make sure that the default is in the list, if not reset it to the first entry.
    oses = Attrib.get("provisioner-available-oses", nr.node).map{ |k,v| k } rescue []
    default = Attrib.get('provisioner-target_os', Role.find_by(name: 'provisioner-os-install')) rescue "fred"
    # If oses is not empty and oses does not include default
    unless oses.empty? or oses.include?(default)
      Attrib.set('provisioner-target_os', Role.find_by(name: 'provisioner-os-install'), oses[0])
    end
    # Extract repos that the provisioner provides out, decide where to store them, and save things.
    new_repos = (nr.wall['rebar']['provisioner']['server']['repositories'] rescue nil)
    Rails.logger.info("base_images: #{new_repos.inspect}")
    return unless new_repos
    pkgsrc_role = Role.find_by!(name: 'rebar-package-sources')
    attr_src = nil
    deployment = nr.deployment
    while attr_src.nil? && !deployment.nil?
      DeploymentRole.find_by(role_id: pkgsrc_role.id, deployment_id: deployment.id)
      deployment = deployment.parent
    end
    attr_src ||= pkgsrc_role
    raise "Cannot find place to stash repos we need to provide!" unless attr_src
    repos_to_add={}
    new_repos.each do |os,repos|
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
    attr_src.with_lock do
      old_repos = Attrib.get('package-repositories',attr_src)
      old_repos.reject!{|e|repos_to_add.keys.member?(e['name'])}
      old_repos.concat(repos_to_add.values)
      Attrib.set('package-repositories',attr_src,old_repos)
    end
  end

end
