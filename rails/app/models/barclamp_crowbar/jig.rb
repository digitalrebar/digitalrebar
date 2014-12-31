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
# This model is a stub for the Jig override system
# It is NOT installed by default, but can be used for testing or as a model

require 'json'
require 'fileutils'

# Script Jig
class BarclampCrowbar::Jig < Jig
  def stage_run(nr)
    super(nr)
  end

  def run(nr,data)
    local_scripts = File.join nr.barclamp.source_path, 'script', 'roles', nr.role.name
    die "No local scripts @ #{local_scripts}" unless File.exists?(local_scripts)
    remote_tmpdir,err,ok = nr.node.ssh("mktemp -d /tmp/scriptjig-XXXXXX")
    remote_tmpdir.strip!
    if remote_tmpdir.empty? || !ok.success?
      die "Did not create remote_tmpdir on #{nr.node.name} for some reason! (#{err})"
    end
    local_tmpdir = %x{mktemp -d /tmp/local-scriptjig-XXXXXX}.strip
    Rails.logger.info("Using local temp dir: #{local_tmpdir}")
    attr_to_shellish(data).each do |k,v|
      target = File.join(local_tmpdir,"attrs",k)
      FileUtils.mkdir_p(target)
      File.open(File.join(target,"attr"),"w") do |f|
        f.printf("%s",v.to_s)
      end
    end
    FileUtils.cp_r(local_scripts,local_tmpdir)
    FileUtils.cp('/opt/opencrowbar/core/script/runner',local_tmpdir)
    out,err,ok = nr.node.scp_to("#{local_tmpdir}/.","#{remote_tmpdir}","-r")
    die("Copy failed! (status = #{$?.exitstatus})\nOut: #{out}\nErr: #{err}") unless ok.success?
    out,err,ok = nr.node.ssh("/bin/bash '#{remote_tmpdir}/runner' '#{remote_tmpdir}' '#{nr.role.name}'")
    die("Script jig run for #{nr.role.name} on #{nr.node.name} failed! (status = #{$?.exitstatus})\nOut: #{out}\nErr: #{err}") unless ok.success?
    nr.update!(runlog: out)
    # Now, we need to suck any written attributes back out.
    new_wall = {}
    out,err,ok = nr.node.scp_from("#{remote_tmpdir}/attrs","#{local_tmpdir}","-r")
    die("Copy of attrs back from #{nr.node.name} failed:\nOut: #{out}\nErr: #{err}") unless ok.success?
    FileUtils.cd(File.join(local_tmpdir,"attrs")) do
      # All new attributes should be saved in wall files.
      Dir.glob("**/wall") do |attrib|
        k = attrib.split('/')[0..-2]
        v = IO.read(attrib).strip
        next if v.empty?
        # Convert well-known strings and strings that look like numbers to JSON values
        v = case
            when v.downcase == "true" then true
            when v.downcase == "false" then false
            when v =~ /^[-+]?[0-9]+$/ then v.to_i
            when v =~ /^[-+]?[0'9a-fA-f]+$/ then v.to_i(16)
            when v =~ /^[-+]?0[bodx]?[0-9a-fA-F]+$/ then v.to_i(0)
            else v
            end
        w = new_wall
        # Build the appropriate hashing structure based on what were directory names.
        k[0..-2].each do |key|
          w[key] ||= Hash.new
          w = w[key]
        end
        w[k[-1]] = v
      end
    end
    nr.update!(wall: new_wall)
    system("sudo -H chown -R crowbar.crowbar #{local_tmpdir}")
    system("sudo rm -rf '#{local_tmpdir}")
    # Clean up after ourselves.
    nr.node.ssh("rm -rf '#{remote_tmpdir}'")
  end

  def create_node(node)
    Rails.logger.info("ScriptJig Creating node: #{node.name}")
    # ? generate a SSH pub/private key pair
    # ? put in node: /user/root authorized_keys file
    # return JSON to be returned to the node
    {}
  end

  def delete_node(node)
    Rails.logger.info("ScriptJig Deleting node: #{node.name}")
  end

  private

  # Turn a nested hash table into an array of key/value pairs
  # This is intended to be turned into a filesystem structure that the
  # scripts being executed on the remote system can access.
  def attr_to_shellish(values, prefix=[])
    res = {}
    if values.kind_of?(Array)
      values.each_index do |i|
        res[i.to_s]=values[i]
      end
      values = res
      res = {}
    end
    values.each do |k,v|
      key = prefix.dup << k.to_s
      case
      when v.nil? then next
      when v.kind_of?(Hash) && !v.empty?
        res.merge!(attr_to_shellish(v,key)) unless v.empty?
      when v.respond_to?(:to_s) then res[key.join("/")] = v.to_s
      end
    end
    res
  end

end
