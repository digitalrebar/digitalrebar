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

class BarclampCluster::MakeCa < LocalRole

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

    runlog = []
    runlog << "Make CA Cert: "
    out, err, status = run_local("sign-it -A -l #{label} -m")
    if !status.success?
      raise "Error: #{out}\n#{err}\n"
    end
    runlog << "Success\n#{out}Save Generated Date:"
    attribname = "#{nr.role.name}-time"
    begin
      # add the time to the node role
      gendate = Time.now.to_s
      Attrib.set(attribname, nr, gendate)
      runlog << "Generated at `#{gendate}`.\n"
    rescue
      runlog << "NOTE: No `#{attribname}` attribute for #{nr.name}, not saving file time value.\nRUNLOG:\n#{runlog}\n"
    end
    runlog << "Success\n#{out}"
    nr.runlog = runlog.join("")
    nr.save!
  end

end

