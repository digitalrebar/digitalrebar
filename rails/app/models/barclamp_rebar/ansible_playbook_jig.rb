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
    status = Open4::popen4ext(true,cmd) do |_pid,stdin,stdout,stderr|
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

    role_group_map = role_yaml['role_group_map']
    role_group_map = {} unless role_group_map
    role_tag_map = role_yaml['role_tag_map']
    role_tag_map = {} unless role_tag_map
    role_role_map = role_yaml['role_role_map']
    role_role_map = {} unless role_role_map

    # Load/Update cache
    cache_dir = '/var/cache/rebar/ansible_playbook'
    FileUtils.mkdir_p(cache_dir)
    role_cache_dir = "#{cache_dir}/#{nr.role.name}"

    File.open("#{cache_dir}/lock", File::RDWR|File::CREAT, 0644) do |f1|
      f1.flock(File::LOCK_EX)

      if !File.exists?(role_cache_dir)
        # If we are told galaxy, then load it into ansible.
        if role_yaml['playbook_src_path'] =~ /^galaxy:/
          out, err, status = exec_cmd("sudo ansible-galaxy install #{role_yaml['playbook_src_path'].split(':')[1]}")
          die "Failed to get @ #{role_file}: #{out} #{err}" unless status.success?

          out, err, status = exec_cmd("mkdir -p #{role_cache_dir}")
          die "Failed to get @ #{role_file}: #{out} #{err}" unless status.success?
        elsif role_yaml['playbook_src_path'] =~ /^http/
          # Load the git cache
          out, err, status = exec_cmd("git clone #{role_yaml['playbook_src_path']} #{role_cache_dir}")
          die "Failed to get @ #{role_file}: #{out} #{err}" unless status.success?
        else
          FileUtils.cp_r("#{local_scripts}/#{role_yaml['playbook_src_path']}/.", "#{role_cache_dir}")
        end
      else
        if role_yaml['playbook_src_path'] =~ /^galaxy:/
          # Update galaxy repos??
        elsif role_yaml['playbook_src_path'] =~ /^http/
          # Update git repos??
        else
          FileUtils.cp_r("#{local_scripts}/#{role_yaml['playbook_src_path']}/.", "#{role_cache_dir}")
        end
      end
    end

    rundir,err,ok = exec_cmd('mktemp -d /tmp/ansible-playbook-jig-XXXXXX')
    rundir.strip!
    if rundir.empty? || !ok.success?
      die "Did not create rundir for some reason! (#{err})"
    end

    # Remap additional variables
    role_yaml['attribute_map'].each do |am|
      next if am['when'] and eval_condition(nr, data, am['when'])
      value = get_value(nr, data, am['name'])
      set_value(data, am['path'], value)
    end if role_yaml['attribute_map']

    File.open("#{rundir}/rebar.json", 'w') do |f|
      JSON.dump(data,f)
    end

    # Build inventory file
    File.open("#{rundir}/inventory.ini", 'w') do |f|
      # all = all nodes in deployment
      nr.deployment.nodes.each do |n|
        address = n.address
        next unless address
        f.write(address.addr + " ansible_ssh_user=root\n")
      end

      nr.deployment.roles.each do |r|
        nrs = NodeRole.peers_by_role(nr.deployment, r)
        next if nrs.nil? or nrs.empty?

        rns = role_group_map[r.name]
        next unless rns
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

    # We don't have a playbook file, then we need to build one.
    if role_yaml['playbook_file'] == '.'
      die "#{nr.role.name} must provide role map" unless role_role_map[nr.role.name]

      role_file = 'cluster.yml'
      File.open("#{role_cache_dir}/#{role_yaml['playbook_path']}/cluster.yml", 'w') do |f|
        f.write("- hosts:\n")
        if role_group_map[nr.role.name]
          role_group_map[nr.role.name].each do |gname|
            f.write("  - #{gname}\n")
          end
        else
          f.write("  - all\n")
        end
        f.write("  roles:\n")
        role_role_map[nr.role.name].each do |rname|
          f.write("  - #{rname}\n")
        end
      end
    else
      role_file = role_yaml['playbook_file']
    end

    rns = role_tag_map[nr.role.name]
    rns_string = ""
    rns_string = "--tags=#{rns.join(',')}" if rns
    out,err,ok = exec_cmd("cd #{role_cache_dir}/#{role_yaml['playbook_path']} ; ansible-playbook -l #{nr.node.address.addr} -i #{rundir}/inventory.ini --extra-vars \"@#{rundir}/rebar.json\" #{role_file} #{rns_string}")
    die("Running: cd #{role_cache_dir}/#{role_yaml['playbook_path']} ; ansible-playbook -l #{nr.node.address.addr} -i #{rundir}/inventory.ini --extra-vars \"@#{rundir}/rebar.json\" #{role_file} #{rns_string}\nScript jig run for #{nr.role.name} on #{nr.node.name} failed! (status = #{$?.exitstatus})\nOut: #{out}\nErr: #{err}") unless ok.success?
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

private

  def get_address(node, command, data)
    args = command.split(/[\)\(\.,]/)
    addr_class = args[1].strip.to_sym
    attr_cat = args[2].strip
    ip_part = args[4].strip

    list = ['admin']
    value = Attrib.get(attr_cat, node)
    list = [value] if value
    addresses = node.addresses(addr_class, list)

    answer = nil
    answer = addresses[0].to_s if ip_part == 'cidr'
    answer = addresses[0].addr if ip_part == 'address'

    # This only works if the node is the starting node.  :-( ??
    if ip_part == 'ifname'
      # check for the address in the detected nic / ip table.
      data.each do |k,v|
        next unless v.is_a?(Hash)
        next unless v['ips']
        ips = v['ips']
        ips.each do |ip_string|
          if ip_string == addresses[0].to_s
            answer = k
            break
          end
        end
        break if answer
      end
    end

    answer
  end

  def get_nodes(nr, command, data)
    parts = command.split(/\./,2)
    new_command = parts[1]

    args = parts[0].split(/[\)\(\.,]/)
    role_name = args[1].strip

    answer = []
    NodeRole.peers_by_role(nr.deployment, Role.find_by_name(role_name)).order(:id).each do |tnr|
      answer << process_command(tnr, new_command, data)
    end

    answer.join(' ')
  end

  def get_first_node(nr, command, data)
    parts = command.split(/\./,2)
    new_command = parts[1]

    args = parts[0].split(/[\)\(\.,]/)
    role_name = args[1].strip

    answer = nil
    tnr = NodeRole.peers_by_role(nr.deployment, Role.find_by_name(role_name)).order(:id).first rescue nil
    answer = process_command(tnr, new_command, data) if tnr

    answer
  end

  # possible custom commands are:
  #   ipaddress([all|v4_only|v6_only], attribute).[ifname|cidr|address]
  #   nodes_with_role(k8scontrail-master).<command applied to all nodes joined by " ">
  #   first_node_with_role(k8scontrail-master).<command applied to node>
  def process_command(nr, command, data)
    answer = nil

    answer = get_address(nr.node, command, data) if command.starts_with?('ipaddress(')
    answer = get_nodes(nr, command, data) if command.starts_with?('nodes_with_role(')
    answer = get_first_node(nr, command, data) if command.starts_with?('first_node_with_role(')

    answer
  end

  # nr is the node role being operated on.
  # Data is a hash that will become JSON at some point.
  # Cond is the eval string
  #    cond is of the form <path> <operator> <value>
  #    path = a / separated set of strings that index into hash and lists.
  #    operator = == or !=
  #    value = string
  #
  # Return boolean value of condition as applied to stringified path results.
  #
  def eval_condition(nr, data, cond)
    args = cond.split
    path = args[0]
    operator = args[1]
    rvalue = args[2]

    lvalue = get_value(nr, data, path)
    lvalue = "" unless lvalue

    case operator
    when '=='
      lvalue.to_s == rvalue
    when '!='
      lvalue.to_s != rvalue
    else
      Rails.logger.info("EvalCondition in Ansible Playbook Jig: #{operator} unknown")
      false
    end
  end


  # nr is the node role being operated on.
  # Data is a hash that will become JSON at some point.
  # Path is string or eval string
  #    path = a / separated set of strings that index into hash and lists.
  #
  #    eval string = eval:<custom string to eval>
  #
  # Return value
  #
  def get_value(nr, data, path)

    # if it is an eval process the command.
    if path.starts_with?('eval:')
      return process_command(nr, path.sub(/^eval:/, ''), data)
    end

    # Assume hash string with possible array indexes
    pieces = path.split('/')
    pieces.each do |p|
      return nil unless data
      if p.include? '['
        p = p.split(/[\]\[]/)

        data = data[p[0]][p[1].to_i]
      else
        data = data[p]
      end
    end
    data
  end

  # Data is a hash that will become JSON at some point.
  # Path is a / separated set of strings that index into hash and lists.
  # Value is what should be set at that path location
  def set_value(data, path, value)
    pieces = path.split('/')
    if pieces.size >= 2
      pieces[0..-1].each do |p|
        return unless data
        if p.include? '['
          p = p.split(/[\]\[]/)

          data = data[p[0]][p[1].to_i]
        else
          data = data[p]
        end
      end
    end

    p = pieces.last
    if p.include? '['
      p = p.split(/[\]\[]/)

      data[p[0]][p[1].to_i] = value
    else
      data[p] = value
    end
  end

end
