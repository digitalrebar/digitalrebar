# Copyright 2015, RackN
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

require 'json'
require 'fileutils'
require 'yaml'

# Ansible Jig
# Instead of calling a playbook call a module
class BarclampRebar::AnsibleJig < Jig

  def exec_cmd(cmd)
    Rails.logger.debug("Local Running #{cmd}")
    out,err = '',''
    status = Open4::popen4ext(true,cmd) do |pid,stdin,stdout,stderr|
      stdin.close
      out << stdout.read
      err << stderr.read
      stdout.close
      stderr.close
    end
    [out,err,status]
  end

  def stage_run(nr)
    super(nr)
  end

  def run(nr,data)
    local_scripts = File.join nr.barclamp.source_path, 'ansible', 'roles', nr.role.name
    die "No local scripts @ #{local_scripts}" unless File.exists?(local_scripts)

    role_file = File.join local_scripts, 'role.yml'
    die "No def file @ #{role_file}" unless File.exists?(role_file)

    role_yaml = YAML.load_file(role_file)
    die "Bad yaml file @ #{role_file}" unless role_yaml

    die "Missing module @ #{role_file}" unless role_yaml['module']
    die "Missing module args @ #{role_file}" unless role_yaml['module_args']

    remote_tmpdir,err,ok = exec_cmd("mktemp -d /tmp/ansible-jig-XXXXXX")
    remote_tmpdir.strip!
    if remote_tmpdir.empty? || !ok.success?
      die "Did not create remote_tmpdir for some reason! (#{err})"
    end

    rundir = "#{remote_tmpdir}/#{nr.role.name}"
    FileUtils.mkdir_p(rundir)

    # Build inventory file
    File.open("#{rundir}/inventory.ini", "w") do |f|
      # all = all nodes in deployment
      nr.deployment.nodes.each do |n|
        address = n.address
        next unless address
        f.write(address.addr + " ansible_ssh_user=root\n")
      end

      nr.deployment.roles.each do |r|
        nrs = NodeRole.peers_by_role(nr.deployment, r)
        next if nrs.nil? or nrs.empty?

        # GREG: Map the roles?
        f.write("[#{r.name}]\n")
        nrs.each do |tnr|
          address = tnr.node.address
          next unless address
          f.write(address.addr + " ansible_ssh_user=root\n")
        end
      end
    end

    out,err,ok = exec_cmd("cd #{rundir} ; ansible all -i inventory.ini -m \"#{role_yaml['module']}\" -a \"#{role_yaml['module_args']}\" -l #{nr.node.address.addr}")
    die("Script jig run for #{nr.role.name} on #{nr.node.name} failed! (status = #{$?.exitstatus})\nOut: #{out}\nErr: #{err}") unless ok.success?
    nr.update!(runlog: out)

    # Now, we need to suck any written attributes back out.
    new_wall = {}
    nr.update!(wall: new_wall)

    # Clean up after ourselves.
    system("rm -rf '#{remote_tmpdir}'")
  end

  def create_node(node)
    Rails.logger.info("AnsibleJig Creating node: #{node.name}")
    # Nothing to do, we build the inventory/all_vars files on the fly
    # return JSON to be returned to the node
    {}
  end

  def delete_node(node)
    # Nothing to do, we build the inventory/all_vars files on the fly
    Rails.logger.info("AnsibleJig Deleting node: #{node.name}")
  end

end
