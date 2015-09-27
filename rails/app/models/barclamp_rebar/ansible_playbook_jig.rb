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

# Ansible Playbook Jig
class BarclampRebar::AnsiblePlaybookJig < Jig

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
    local_scripts = File.join nr.barclamp.source_path, 'ansible-playbooks', 'roles', nr.role.name
    die "No local scripts @ #{local_scripts}" unless File.exists?(local_scripts)

    role_file = File.join local_scripts, 'role.yml'
    die "No def file @ #{role_file}" unless File.exists?(role_file)

    role_yaml = YAML.load_file(role_file)
    die "Bad yaml file @ #{role_file}" unless role_yaml

    die "Missing src path @ #{role_file}" unless role_yaml['playbook_src_path']
    die "Missing playbook path @ #{role_file}" unless role_yaml['playbook_path']
    die "Missing playbook file @ #{role_file}" unless role_yaml['playbook_file']

    role_map = role_yaml['role_map']
    role_map = {} unless role_map

    # Load/Update cache
    cache_dir = "/var/cache/rebar/ansible_playbook"
    FileUtils.mkdir_p(cache_dir)
    role_cache_dir = "#{cache_dir}/#{nr.role.name}"

    File.open("#{cache_dir}/lock", File::RDWR|File::CREAT, 0644) do |f1|
      f1.flock(File::LOCK_EX)

      unless File.exists?("#{role_cache_dir}")
        if role_yaml['playbook_src_path'] =~ /^http/
          # Load the git cache
          out, err, status = exec_cmd("git clone #{role_yaml['playbook_src_path']} #{role_cache_dir}")
          die "Failed to get @ #{role_file}: #{out} #{err}" unless status.success?
        else
          FileUtils.cp_r("#{local_scripts}/#{role_yaml['playbook_src_path']}/.", "#{role_cache_dir}")
        end
      else
        if role_yaml['playbook_src_path'] =~ /^http/
          # Update git repos??
        else
          FileUtils.cp_r("#{local_scripts}/#{role_yaml['playbook_src_path']}/.", "#{role_cache_dir}")
        end
      end
    end

    rundir,err,ok = exec_cmd("mktemp -d /tmp/ansible-playbook-jig-XXXXXX")
    rundir.strip!
    if rundir.empty? || !ok.success?
      die "Did not create rundir for some reason! (#{err})"
    end

    File.open("#{rundir}/rebar.json","w") do |f|
      JSON.dump(data,f)
    end

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

        rns = role_map[r.name]
        rns = [ r.name ] unless rns
        rns.each do |rn|
          next if rn == 'all'
          f.write("[#{rn}]\n")
          nrs.each do |tnr|
            address = tnr.node.address
            next unless address
            f.write(address.addr + " ansible_ssh_user=root\n")
          end
        end
      end
    end

    rns = role_map[nr.role.name]
    rns = [ nr.role.name ] unless rns
    out,err,ok = exec_cmd("cd #{role_cache_dir}/#{role_yaml['playbook_path']} ; ansible-playbook -l #{nr.node.address.addr} -i #{rundir}/inventory.ini --extra-vars \"@#{rundir}/rebar.json\" #{role_yaml['playbook_file']} --tags=#{rns.join(',')}")
    die("Running: cd #{role_cache_dir}/#{role_yaml['playbook_path']} ; ansible-playbook -l #{nr.node.address.addr} -i #{rundir}/inventory.ini --extra-vars \"@#{rundir}/rebar.json\" #{role_yaml['playbook_file']} --tags=#{rns.join(',')}\nScript jig run for #{nr.role.name} on #{nr.node.name} failed! (status = #{$?.exitstatus})\nOut: #{out}\nErr: #{err}") unless ok.success?
    nr.update!(runlog: out)

    # Now, we need to suck any written attributes back out.
    new_wall = {}
    nr.update!(wall: new_wall)

    # Clean up after ourselves.
    system("rm -rf '#{rundir}'")
  end

  def create_node(node)
    Rails.logger.info("AnsiblePlaybookJig Creating node: #{node.name}")
    # Nothing to do, we build the inventory/all_vars files on the fly
    # return JSON to be returned to the node
    {}
  end

  def delete_node(node)
    # Nothing to do, we build the inventory/all_vars files on the fly
    Rails.logger.info("AnsiblePlaybookJig Deleting node: #{node.name}")
  end

end
