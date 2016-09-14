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

class BarclampCluster::InstallRootCa < LocalRole

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
    root_info = data["cert"]["root"]

    runlog = []
    tmpfile=Dir::Tmpname.make_tmpname("/tmp/data", "rootca")
    runlog << "Get Root CA Pem: "
    out,err,status = run_local("sign-it -A -l #{label} -i -o #{tmpfile}")
    if !status.success?
      raise "Failed to get cert for #{label}\n#{out}\n#{err}\n"
    end 
    runlog << "Success\n#{out}Remove Cert on Node:"
    out,err,status = nr.node.run("rm -f #{root_info["destination"]}")
    if !status.success?
      raise "Failed to remove #{root_info['destination']}\n#{out}\n#{err}\n"
    end 
    runlog << "Success\n#{out}Copy Cert to Node:"
    out,err,status = nr.node.transfer().copy_to("#{tmpfile}.pem", root_info['destination'])
    if !status.success?
      raise "Failed to copy cert to #{root_info['destination']}\n#{out}\n#{err}\n"
    end 
    runlog << "Success\n#{out}Chmod Cert on Node:"
    out,err,status = nr.node.run("chmod #{root_info["perm"]} #{root_info["destination"]}")
    if !status.success?
      raise "Failed to chmod #{root_info['destination']}\n#{out}\n#{err}\n"
    end 
    runlog << "Success\n#{out}Chown Cert on Node:"
    out,err,status = nr.node.run("chown #{root_info["user"]}:#{root_info["group"]} #{root_info["destination"]}")
    if !status.success?
      raise "Failed to chown #{root_info['destination']}\n#{out}\n#{err}\n"
    end 
    runlog << "Success\n#{out}Remove tmpfiles:"
    out,err,status = run_local("rm -rf #{tmpfile}*", nr)
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

