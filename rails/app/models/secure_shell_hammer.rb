# Copyright 2014 Victor Lowther
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

class SecureShellHammer < Hammer

  # For now, assume that the SSH power manager is always applicable.
  # This may change if we start having non-Linux nodes.
  def self.probe(node)
    true
  end

  def actions
    { power: [:reboot],
      xfer: [:copy_from, :copy_to],
      run: [:run] }
  end

  def run(cmd)
    throw "ERROR: SecureShellHammer.run cannot determine address for #{node.name}" unless node.address
    run_on(". /etc/profile; exec ssh -l #{username} #{node.address.addr} -- #{cmd}")
  end

  def copy_from(remote_src, local_dest, opts="")
    run_on("exec scp #{opts} root@[#{node.address.addr}]:#{remote_src} #{local_dest}")
  end

  def copy_to(local_src, remote_dest, opts="")
    run_on("exec scp #{opts} #{local_src} root@[#{node.address.addr}]:#{remote_dest}")
  end

  # and it only allows you to reboot a node via its "reboot" command
  def reboot
    node.update!(alive: false)
    run("reboot")
  end

  def run_on(cmd)
    Rails.logger.debug("Node: #{node.name}: Running #{cmd}")
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
end
