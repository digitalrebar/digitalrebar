# Copyright 2013, Dell
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

class BarclampProvisioner::Server < Role

  KEY_FILE = "/etc/crowbar.install.key"

  # when proposed, we need to create an entry for crowbar's admin key
  def on_proposed(nr)

    # the key should exist, we need to handle deal with cases where it does not
    key = if File.exist? KEY_FILE
      File.read(KEY_FILE).chomp.strip
    elsif Jig.active('test')
      "testing_no_key_required"
    else
      Rails.logger.error "Provisioner Server Role requires the Crowbar Key from '#{KEY_FILE}' but could not find it"
      raise "Proviser Server Role cannot find #{KEY_FILE}"
    end

    # build the JSON data for the node role
    nr.sysdata = {
      "crowbar" => {
        "provisioner" => {
          "machine_key" => key
        }
      }
    }

  end

end
