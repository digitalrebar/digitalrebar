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

  after_create      :load_uuid

  def load_uuid
    self.reload
  end

  private :load_uuid
  
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

  def run(cmd, nr = nil)
    node_address = node.address
    throw "ERROR: SecureShellHammer.run cannot find a reachable address for #{node.name}" unless node_address
    run_on(". /etc/profile; exec ssh -l #{username} #{node_address.addr} -- #{cmd}", nr)
  end

  def copy_from(remote_src, local_dest, opts="", nr=nil)
    run_on("exec scp #{opts} root@[#{node.address.addr}]:#{remote_src} #{local_dest}", nr)
  end

  def copy_to(local_src, remote_dest, opts="", nr=nil)
    run_on("exec scp #{opts} #{local_src} root@[#{node.address.addr}]:#{remote_dest}", nr)
  end

  # and it only allows you to reboot a node via its "reboot" command
  def reboot
    node.update!(alive: false)
    run("reboot")
  end

  def run_on(cmd, nr = nil)
    Rails.logger.debug("Node: #{node.name}: Running #{cmd}")
    out,err = '',''
    status = Open4::popen4ext(true,cmd) do |pid,stdin,stdout,stderr|
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
end
