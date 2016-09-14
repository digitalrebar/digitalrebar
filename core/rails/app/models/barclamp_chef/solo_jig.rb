# Copyright 2014, Dell
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

require 'json'
require 'fileutils'

class BarclampChef::SoloJig < Jig

  def make_run_list(nr)
    runlist = Array.new
    runlist << "recipe[barclamp]"
    runlist << "recipe[ohai]"
    runlist << "recipe[utils]"
    runlist << "role[#{nr.role.name}]"
    runlist << "recipe[rebar-hacks::solo-saver]"
    Rails.logger.info("Chef Solo: discovered run list: #{runlist}")
    return runlist
  end

  def on_disk_name
    "chef"
  end

  def stage_run(nr)
    return {
      "name" => "rebar_baserole",
      "default_attributes" => super(nr),
      "override_attributes" => {},
      "json_class" => "Chef::Role",
      "description" => "Rebar role to provide default attribs for this run",
      "chef-type" => "role",
      "run_list" => make_run_list(nr)
    }
  end

  def run (nr,data)
    local_tmpdir = %x{mktemp -d /tmp/local-chefsolo-XXXXXX}.strip
    #remote_tmpdir = %x{mktemp -d /tmp/chefsolo-XXXXXX}.strip
    chef_path = File.join(nr.barclamp.source_path, on_disk_name)
    role_json = File.join(local_tmpdir,"rebar_baserole.json")
    node_json = File.join(local_tmpdir,"node.json")
    unless File.directory?(chef_path)
      raise("No Chef data at #{chef_path}")
    end
    paths = ["#{chef_path}/roles",
             "#{chef_path}/data_bags",
             "/var/tmp/barclamps/#{nr.role.barclamp.name}/chef"].select{|d|File.directory?(d)}.join(' ')
    # This needs to be replaced by rsync.
    #out,err,ok = nr.node.scp_to(paths,remote_tmpdir,"-r")
    out,err,ok = nr.node.scp_to(paths,"/var/chef","-r")
    raise("Chef Solo jig run for #{nr.name} failed to copy Chef information from #{paths.inspect}\nOut: #{out}\nErr: #{err}") unless ok.success?

    Rails.logger.debug("Chef Solo Jig: #{nr.name} mkdir time start: #{Time.now.to_s}")
    #out,err,ok = nr.node.ssh("mkdir -p #{remote_tmpdir}/cookbooks")
    out,err,ok = nr.node.ssh("mkdir -p /var/chef/cookbooks")
    raise("Chef Solo jig run for #{nr.name} failed to make cookbooks dir.\nOut: #{out}\nErr:#{err}") unless ok.success?

    Rails.logger.debug("Chef Solo Jig: #{nr.name} scp time start: #{Time.now.to_s}")
    cookbook_tarball = "/var/cache/rebar/cookbooks/package.tar.gz"
    raise("Missing Chef cookbooks in #{cookbook_tarball}") unless File.file?(cookbook_tarball)
    out,err,ok = nr.node.scp_to(cookbook_tarball,"/var/chef/")
    raise("Chef Solo jig run for #{nr.name} failed to scp pacakge.tar.gz.\nOut: #{out}\nErr:#{err}\n") unless ok.success?
    Rails.logger.debug("Chef Solo Jig: #{nr.name} tar time start: #{Time.now.to_s}")
    out,err,ok = nr.node.ssh("/bin/tar xvzf /var/chef/package.tar.gz -C /var/chef 2>&1")
    raise("Chef Solo jig run for #{nr.name} failed to untar package.tar.gz\nOut: #{out}\nErr:#{err}\n") unless ok.success?
    #Rails.logger.info("Chef Solo jig run for #{nr.name} cookbook tarball tar output: \nOut: #{out}\nClass: #{out.class}\n")
    Rails.logger.debug("Chef Solo Jig: #{nr.name} scp/tar time end: #{Time.now.to_s}")

    File.open(role_json,"w") do |f|
      f.write(JSON.pretty_generate(data))
    end
    File.open(node_json,"w") do |f|
      JSON.dump({"run_list" => "role[rebar_baserole]"},f)
    end
    if nr.role.respond_to?(:jig_role) && !File.exists?("#{chef_path}/roles/#{nr.role.name}.rb")
      # Create a JSON version of the role we will need so that chef solo can pick it up
      File.open("#{local_tmpdir}/#{nr.role.name}.json","w") do |f|
        JSON.dump(nr.role.jig_role(nr),f)
      end
      out,err,ok = nr.node.scp_to("#{local_tmpdir}/#{nr.role.name}.json","/var/chef/roles/#{nr.role.name}.json")
      raise("Chef Solo jig: #{nr.name}: failed to copy dynamic role to target\nOut: #{out}\nErr:#{err}") unless ok.success?
    end
    out,err,ok = nr.node.scp_to(role_json, "/var/chef/roles/rebar_baserole.json")
    raise("Chef Solo jig: #{nr.name}: failed to copy node attribs to target\nOut: #{out}\nErr:#{err}") unless ok.success?
    out,err,ok = nr.node.scp_to(node_json, "/var/chef/node.json")
    raise ("Chef Solo jig: #{nr.name}: failed to copy node to target\nOut: #{out}\nErr:#{err}") unless ok.success?
    out,err,ok = nr.node.ssh("chef-solo -j /var/chef/node.json", nr)
    raise("Chef Solo jig run for #{nr.name} failed\nOut: #{out}\nErr:#{err}") unless ok.success?
    nr.update!(runlog: out)
    node_out_json = File.join(local_tmpdir, "node-out.json")
    out,err,ok = nr.node.scp_from("/var/chef/node-out.json",local_tmpdir)
    raise("Chef Solo jig run for #{nr.name} did not copy attributes back\nOut: #{out}\nErr:#{err}") unless ok.success?
    from_node = JSON.parse(IO.read(node_out_json))
    nr.update!(wall: from_node["normal"])
    nr.node.discovery_merge({"ohai" => from_node["automatic"]})
  end
end
    
