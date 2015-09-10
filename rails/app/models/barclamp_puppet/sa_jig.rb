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

class BarclampPuppet::SaJig < Jig
  def stage_run(nr)
    super(nr)
  end

  def run(nr,data)
    # find puppet manifest script(s) for this role
    local_scripts = File.join nr.barclamp.source_path, 'puppet', 'roles', nr.role.name
    die "No local manifests @ #{local_scripts}" unless File.exists?(local_scripts)

    # make remote tmp dir
    remote_tmpdir,err,ok = nr.node.ssh("mktemp -d /tmp/puppetsajig-XXXXXX")
    remote_tmpdir.strip!
    if remote_tmpdir.empty? || !ok.success?
      die "Did not create remote_tmpdir on #{nr.node.name} for some reason! (#{err})"
    end

    # make local tmp dir
    local_tmpdir = %x{mktemp -d /tmp/local-puppetsajig-XXXXXX}.strip
    Rails.logger.info("Using local temp dir: #{local_tmpdir}")

    # copy scripts, runner, and modules to local tmp dir
    FileUtils.cp_r(local_scripts,local_tmpdir)
    FileUtils.cp('/opt/digitalrebar/core/puppet/roles/runner',local_tmpdir)
    target = File.join(local_tmpdir,"modules")
    FileUtils.mkdir_p(target)
    module_src_dir = File.join nr.barclamp.source_path, 'puppet', 'modules'
    includes = `grep -h "^include" #{local_scripts}/*`.split("\n").map{|m| /.* (.*)$/.match(m)[-1]}
    includes.each do |m|
      mod = File.join(module_src_dir, m)
      die("Could not find included module #{m} for #{nr.role.name} puppet run!") unless File.exists? mod
      FileUtils.cp_r(File.join(module_src_dir, m), target)
    end

    # copy all staged files to remote tmp dir
    out,err,ok = nr.node.scp_to("#{local_tmpdir}/.","#{remote_tmpdir}","-r")
    die("Copy failed! (status = #{$?.exitstatus})\nOut: #{out}\nErr: #{err}") unless ok.success?

    # execute the puppet runs
    out,err,ok = nr.node.ssh("/bin/bash '#{remote_tmpdir}/runner' '#{remote_tmpdir}' '#{nr.role.name}'")
    die("Stand-alone puppet jig run for #{nr.role.name} on #{nr.node.name} failed! (status = #{$?.exitstatus})\nOut: #{out}\nErr: #{err}") unless ok.success?
    nr.update!(runlog: out)

    system("sudo -H chown -R rebar.rebar #{local_tmpdir}")
    system("sudo rm -rf '#{local_tmpdir}")
    # Clean up after ourselves.
    nr.node.ssh("rm -rf '#{remote_tmpdir}'")
  end

end
