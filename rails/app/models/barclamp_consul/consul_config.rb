# Copyright 2014, Victor Lowther
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

require 'resolv'

class BarclampConsul::ConsulConfig < Role

  def on_deployment_create(dr)
    DeploymentRole.transaction do
      if Attrib.get("consul-encrypt",dr) == "change_me"
        Attrib.set("consul-encrypt",dr,SecureRandom.base64)
      end
      if Attrib.get("consul-acl-master-token",dr) == "change_me"
        Attrib.set("consul-acl-master-token",dr,SecureRandom.uuid)
      end
    end
  end

end
