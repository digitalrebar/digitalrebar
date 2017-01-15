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

  CACHE_DIR = '/var/cache/rebar/ansible_playbook'

  def exec_cmd(cmd, nr = nil)
    Rails.logger.debug("Local Running #{cmd}")
    out,err = '',''
    status = Open4::popen4ext(true,cmd) do |_pid,stdin,stdout,stderr|
      stdin.close

      begin
        # Readpartial will read all the data up to 16K
        # If not data is present, it will block
        loop do
          out << stdout.readpartial(16384)
          nr.update!(runlog: out) if nr
        end
      rescue Errno::EAGAIN
        retry
      rescue EOFError
      end

      err << stderr.read

      stdout.close
      stderr.close
    end
    [out,err,status]
  end

  def stage_run(nr)
    super(nr)
  end

  def node_inventory_data(node, nr, data, inventory_map)
    address = node.address
    return nil unless address

    answer = node.name + " ansible_ssh_host=" + address.addr + " ansible_ssh_user=root"

    # Process inventory map
    #
    # Get per node data
    data = stage_run(node.node_roles.first)
    inventory_map.each do |am|
      next if am['when'] and !eval_condition(node, nr, data, am['when'])
      value = get_value(node, nr, data, am['name'])
      answer += " #{am['path']}=#{value}"
    end

    answer += "\n"
    answer
  end

  def run(nr,data)

    # pull metadata from barclamp
    role_yaml = nr.role.barclamp.metadata || {}
    # override with role metadata
    role_yaml.merge! nr.role.metadata

    die "Missing src path @ #{nr.role.name}" unless role_yaml['playbook_src_paths']
    die "Missing playbook path @ #{nr.role.name}" unless role_yaml['playbook_path']
    die "Missing playbook file @ #{nr.role.name}" unless role_yaml['playbook_file']

    role_group_map = role_yaml['role_group_map']
    role_group_map = {} unless role_group_map
    role_tag_map = role_yaml['role_tag_map']
    role_tag_map = {} unless role_tag_map
    role_role_map = role_yaml['role_role_map']
    role_role_map = {} unless role_role_map
    inventory_map = role_yaml['inventory_map']
    inventory_map = {} unless inventory_map

    # Load/Update cache
    FileUtils.mkdir_p(CACHE_DIR)
    role_cache_dir = CACHE_DIR
    # Allow multiple playbooks in a single repo
    role_cache_dir += "/#{nr.role.name}" unless role_yaml['playbook_combined_repo']

    File.open("#{CACHE_DIR}/lock", File::RDWR|File::CREAT, 0644) do |f1|
      f1.flock(File::LOCK_EX)

      # Put source in place
      role_yaml['playbook_src_paths'].each do |dir, info|
        piece_part = "#{role_cache_dir}/#{dir}"

        # generate role files if they are in the metadata
        if info == "metadata"
          # clear past run because we replace the file each time
          out, err, status = exec_cmd("rm -rf #{piece_part}")
          role_yaml["files"].each do |item|
            type = item["type"] || "tasks"
            out, err, status = exec_cmd("mkdir -p #{piece_part}/#{type}")
            # add endings if missing
            file = item["file"] || "main"
            f = if file =~ /.yml$|.j2$/
              file
            elsif type == 'templates'
              file + ".j2"
            else
              file + ".yml"
            end
            f2 = File.open("#{piece_part}/#{type}/#{f}", "w")
            body = item["body"]
            # allow body to be array for readability
            body = body.join("\n") if body.kind_of?(Array)
            f2.write(body)
            f2.close
          end
        end

        if !File.exists?(piece_part)
          # If we are told galaxy, then load it into ansible.
          if info =~ /^galaxy:/
            out, err, status = exec_cmd("sudo ansible-galaxy install #{info.split(':')[1]}")
            die "Failed to get #{info} @ #{nr.role.name}: #{out} #{err}" unless status.success?
            out, err, status = exec_cmd("mkdir -p #{piece_part}")
            die "Failed to mkdir @ #{nr.role.name}:#{dir} #{out} #{err}" unless status.success?
          elsif info =~ /^http/
            parts = info.split(' ')
            branch = "-b #{parts[1]}" if parts[1] and !parts[1].empty?
            out, err, status = exec_cmd("mkdir -p #{role_cache_dir}")
            die "Failed to mkdir @ #{nr.role.name}:#{dir} #{out} #{err}" unless status.success?
            # Load the git cache
            out, err, status = exec_cmd("git clone #{branch} #{parts[0]} #{piece_part}")
            die "Failed to git #{info} @ #{nr.role.name}: #{out} #{err}" unless status.success?
          else
            local_scripts = File.join nr.barclamp.source_path, 'ansible-playbooks', 'roles', nr.role.name
            FileUtils.cp_r("#{local_scripts}/#{info}/.", "#{piece_part}")
          end
        else
          if info =~ /^galaxy:/
            # Update galaxy repos??
          elsif info =~ /^http/
            # Update git repos??
          elsif info =~ /^metadata$/
            # action is above, do nothing
          else
            local_scripts = File.join nr.barclamp.source_path, 'ansible-playbooks', 'roles', nr.role.name
            FileUtils.cp_r("#{local_scripts}/#{info}/.", "#{piece_part}")
          end
        end
      end

      # Pieces in place run actions
      if role_yaml['playbook_src_setup']
        role_yaml['playbook_src_setup'].each do |action|
          out, err, status = exec_cmd("cd #{role_cache_dir}; #{action}")
          die "Failed to setup @ #{action}: #{out} #{err}" unless status.success?
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
      next if am['when'] and !eval_condition(nr.node, nr, data, am['when'])
      value = get_value(nr.node, nr, data, am['name'])
      set_value(data, am['path'], value)
    end if role_yaml['attribute_map']

    # we don't want null values the data set because Ansible prefers them to just be undefined (missing)
    data.delete_if {|key, value| value.nil? }

    File.open("#{rundir}/rebar.json", 'w') do |f|
      JSON.dump(data,f)
    end

    # Build inventory file
    File.open("#{rundir}/inventory.ini", 'w') do |f|
      # all = all nodes in deployment
      nr.deployment.nodes.each do |n|
        ninfo = node_inventory_data(n, nr, data, inventory_map)
        f.write(ninfo) if ninfo
      end

      groups = {}
      nr.deployment.roles.each do |r|
        nrs = NodeRole.peers_by_role(nr.deployment, r)
        next if nrs.nil? or nrs.empty?

        rns = role_group_map[r.name]
        next unless rns
        rns.each do |rn|
          next if rn == 'all'
          groups[rn] ||= []
          nrs.each do |tnr|
            ninfo = node_inventory_data(tnr.node, nr, data, inventory_map)
            groups[rn] << ninfo if ninfo
          end
        end
      end

      groups.each do |g,list|
        f.write("[#{g}]\n")
        list.uniq.each do |ninfo|
          f.write(ninfo)
        end
      end
    end

    # We don't have a playbook file, then we need to build one.
    clean_role_file = false
    if role_yaml['playbook_file'] == '.'
      die "#{nr.role.name} must provide role map" unless role_role_map[nr.role.name]

      clean_role_file = true
      role_file = "cluster-apj-#{nr.id}.yml"
      File.open("#{role_cache_dir}/#{role_yaml['playbook_path']}/#{role_file}", 'w') do |f|
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
          if rname.is_a?(String)
            f.write("  - #{rname}\n")
          else
            f.write("  - { role: \"#{rname["role"]}\", when: \"#{rname["when"]}\" }\n")
          end
        end
      end
    else
      role_file = role_yaml['playbook_file']
    end

    rns = role_tag_map[nr.role.name]
    rns_string = ""
    rns_string = "--tags=#{rns.join(',')}" if rns

    # Run one node (Default)
    nodestring = "-l #{nr.node.name}"
    # unless deplyoment scope - then run all nodes
    nodestring = "" if role_yaml['playbook_scope'] == 'deployment'

    out,err,ok = exec_cmd("cd #{role_cache_dir}/#{role_yaml['playbook_path']} ; ansible-playbook #{nodestring} -i #{rundir}/inventory.ini --extra-vars \"@#{rundir}/rebar.json\" #{role_file} #{rns_string}", nr)
    die("Running: cd #{role_cache_dir}/#{role_yaml['playbook_path']} ; ansible-playbook #{nodestring} -i #{rundir}/inventory.ini --extra-vars \"@#{rundir}/rebar.json\" #{role_file} #{rns_string}\nScript jig run for #{nr.role.name} on #{nr.node.name} failed! (status = #{$?.exitstatus})\nOut: #{out}\nErr: #{err}") unless ok.success?
    nr.update!(runlog: out)

    # Now, we need to suck any written attributes back out.
    new_wall = {}
    nr.update!(wall: new_wall)

    # Clean up after ourselves.
    system("rm -rf '#{rundir}'")
    system("rm -f '#{role_cache_dir}/#{role_yaml['playbook_path']}/#{role_file}'") if clean_role_file
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

  # remove playbook downloads so system gets fresh copy
  def flush()
    system("rm -rf '#{CACHE_DIR}'") rescue Rails.logger.info("AnsiblePlaybookJig did NOT flush playbooks from #{CACHE_DIR}")
    Rails.logger.info("AnsiblePlaybookJig flushed playbooks from #{CACHE_DIR}")
  end

private

  def walk_nics(node, ip_str)
    # check for the address in the detected nic / ip table.
    answer = nil
    nic_data = Attrib.get('nics', node)
    return nil unless nic_data
    nic_data.each do |k,v|
      next unless v.is_a?(Hash)
      next unless v['ips']
      ips = v['ips']
      ips.each do |ip_string|
        if ip_string.split('/')[0] == ip_str
          answer = k
          break
        end
      end
      return answer if answer
    end
    answer
  end

  def get_address(node, nr, command)
    args = command.split(/[\)\(\.,]/)
    addr_class = args[1].strip.to_sym
    attr_cat = args[2].strip

    fallback_choice = :all
    if args.length == 5
      ip_part = args[4].strip
    else
      ip_part = args[5].strip
      fallback_choice = args[3].strip.to_sym
    end

    list = ['admin']
    value = Attrib.get(attr_cat, nr)
    list = [value] if value
    addresses = node.addresses(addr_class, list, fallback_choice)

    answer = nil
    answer = addresses[0].to_s if ip_part == 'cidr'
    answer = addresses[0].addr if ip_part == 'address'

    if ip_part == 'ifname'
      answer = walk_nics(node, addresses[0].addr)
      # This is a hack for kubernetes until we get a better handle on networking.
      unless answer
        value = Attrib.get('node-private-control-address', node)
        if value
          answer = walk_nics(node, IP.coerce(value).addr)
        end
      end
    end

    answer
  end

  def count_nodes(node, nr, command)
    parts = command.split(/\./,2)
    new_command = parts[1]

    args = parts[0].split(/[\)\(\.,]/)
    role_name = args[1].strip

    NodeRole.peers_by_role(node.deployment, Role.find_by_name(role_name)).count()
  end

  def get_nodes(node, nr, command)
    parts = command.split(/\./,2)
    new_command = parts[1]

    args = parts[0].split(/[\)\(\.,]/)
    role_name = args[1].strip

    answer = []
    NodeRole.peers_by_role(node.deployment, Role.find_by_name(role_name)).order(:node_id).each do |tnr|
      answer << process_command(tnr.node, nr, new_command)
    end

    answer.join(' ')
  end

  def get_first_node(node, nr, command)
    parts = command.split(/\./,2)
    new_command = parts[1]

    args = parts[0].split(/[\)\(\.,]/)
    role_name = args[1].strip

    answer = nil
    tnr = NodeRole.peers_by_role(node.deployment, Role.find_by_name(role_name)).order(:node_id).first rescue nil
    answer = process_command(tnr.node, nr, new_command) if tnr

    answer
  end

  def get_attrib(node, nr, command)
    args = command.split(/[\)\(\.,]/)
    val = Attrib.get(args[1], node)

    # It wans an IP fix.
    if val and args.length > 3
      ip = IP.coerce(val)
      val = ip.addr if args[3] == 'address'
      val = ip.to_s if args[3] == 'cidr'
    end

    val
  end

  # possible custom commands are:
  #   ipaddress([all|v4_only|v6_only], attribute, [default,public,private]).[ifname|cidr|address]
  #   nodes_with_role(k8scontrail-master).<command applied to all nodes joined by " ">
  #   first_node_with_role(k8scontrail-master).<command applied to node>
  def process_command(node, nr, command)
    answer = nil

    answer = get_attrib(node, nr, command) if command.starts_with?('attrib(')
    answer = get_address(node, nr, command) if command.starts_with?('ipaddress(')
    answer = get_nodes(node, nr, command) if command.starts_with?('nodes_with_role(')
    answer = count_nodes(node, nr, command) if command.starts_with?('count_nodes_with_role(')
    answer = get_first_node(node, nr, command) if command.starts_with?('first_node_with_role(')

    answer
  end

  # node is the node being operated on.
  # data is the data to look through
  # Cond is the eval string
  #    cond is of the form <path> <operator> <value>
  #    path = a / separated set of strings that index into hash and lists.
  #    operator = == or !=
  #    value = string
  #
  # Return boolean value of condition as applied to stringified path results.
  #
  def eval_condition(node, nr, data, cond)
    args = cond.split
    path = args[0]
    operator = args[1]
    rvalue = args[2]

    lvalue = get_value(node, nr, data, path)
    lvalue = "" if lvalue.nil?

    # Return the eval
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


  # Node is the node being operated on.
  # Data is a hash that will become JSON at some point.
  # Path is string or eval string
  #    path = a / separated set of strings that index into hash and lists.
  #
  #    eval string = eval:<custom string to eval>
  #
  # Return value
  #
  def get_value(node, nr, data, path)

    # if it is an eval process the command.
    if path.starts_with?('eval:')
      return process_command(node, nr, path.sub(/^eval:/, ''))
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
      pieces[0..-2].each do |p|
        return unless data
        if p.include? '['
          p = p.split(/[\]\[]/)

          tdata = data[p[0]][p[1].to_i]
	  if tdata.nil?
            data[p[0]][p[1].to_i] = {}
            tdata = data[p[0]][p[1].to_i]
	  end
	  data = tdata
        else
          tdata = data[p]
	  if tdata.nil?
	    data[p] = {}
	    tdata = data[p]
	  end
	  data = tdata
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
