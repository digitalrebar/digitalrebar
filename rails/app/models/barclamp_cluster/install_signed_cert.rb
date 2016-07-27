# Copyright 2016, RackN
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

class BarclampCluster::InstallSignedCert < LocalRole

  #
  # This is a meta role that can be used to make ca roots.
  #
  # The system uses trust-me to build the root key and cert and that is stored in consul.  The label is
  # in the <rolename>-label variable
  #
  # The install-root-cert role will use the <rolename>-root-cert to install the cert
  # into the filesystem.
  #
  # The install-signed-cert role will generate a signed cert from the root and place it in the filesystem
  #

  def do_transition(nr, data)
    label = data["cert"]["label"]
    key_info = data["cert"]["key"]
    cert_info = data["cert"]["cert"]

    runlog = []
    tmpfile=Dir::Tmpname.make_tmpname("/tmp/data", "signed")
    runlog << "Get key/cert pair: "

    hosts = [ nr.node.name ]
    hosts << NetworkAllocation.where(node_id: nr.node.id).map{|a|a.address.addr}
    hosts.flatten!
    out,err,status = run_local("sign-it -A -l #{label} -s -o #{tmpfile} -h #{hosts.join(",")}")
    if !status.success?
      raise "Failed to sign cert for #{label}\n#{out}\n#{err}\n"
    end
    runlog << "Success\n#{out}Remove cert on node: "
    out,err,status = nr.node.run("rm -f #{cert_info["destination"]}")
    if !status.success?
      raise "Failed to remove cert\n#{out}\n#{err}\n"
    end
    runlog << "Success\n#{out}Copy cert to node: "
    out,err,status = nr.node.transfer().copy_to("#{tmpfile}.pem", cert_info['destination'])
    if !status.success?
      raise "Failed to copy cert\n#{out}\n#{err}\n"
    end
    runlog << "Success\n#{out}Chmod cert on node: "
    out,err,status = nr.node.run("chmod #{cert_info["perm"]} #{cert_info["destination"]}")
    if !status.success?
      raise "Failed to chmod cert\n#{out}\n#{err}\n"
    end
    runlog << "Success\n#{out}Chown cert on node: "
    out,err,status = nr.node.run("chown #{cert_info["user"]}:#{cert_info["group"]} #{cert_info["destination"]}")
    if !status.success?
      raise "Failed to chown cert\n#{out}\n#{err}\n"
    end
    runlog << "Success\n#{out}rm key on node: "
    out,err,status = nr.node.run("rm -f #{key_info["destination"]}")
    if !status.success?
      raise "Failed to chown cert\n#{out}\n#{err}\n"
    end
    runlog << "Success\n#{out}Copy key to node: "
    out,err,status = nr.node.transfer().copy_to("#{tmpfile}.key", key_info['destination'])
    if !status.success?
      raise "Failed to copy key\n#{out}\n#{err}\n"
    end
    runlog << "Success\n#{out}Chmod key on node: "
    out,err,status = nr.node.run("chmod #{key_info["perm"]} #{key_info["destination"]}")
    if !status.success?
      raise "Failed to chmod key\n#{out}\n#{err}\n"
    end
    runlog << "Success\n#{out}Chown key on node: "
    out,err,status = nr.node.run("chown #{key_info["user"]}:#{key_info["group"]} #{key_info["destination"]}")
    if !status.success?
      raise "Failed to chmod key\n#{out}\n#{err}\n"
    end
    runlog << "Success\n#{out}Rmove tmpfiles: "
    out,err,status = run_local("rm -rf #{tmpfile}*")
    if !status.success?
      raise "Failed to remove tmpfiles\n#{out}\n#{err}\n"
    end
    runlog << "Success\n#{out}"
    NodeRole.transaction do
      nr.runlog = runlog.join("")
      nr.save!
    end
  end

end

