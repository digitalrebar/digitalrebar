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

class BarclampProvisioner::DockerSetup < Role

  def on_node_create(node)
    rerun_my_noderoles
  end

  def on_node_change(node)
    rerun_my_noderoles
  end

  def on_node_delete(node)
    rerun_my_noderoles
  end

  #
  # XXX: let the created docker node pass in the image name, maybe?
  #
  def rerun_my_noderoles
    to_enqueue = []
    hosts = {}
    ActiveRecord::Base.connection.execute("select * from docker_database order by name asc, address asc").each do |row|
      name,address,image = row["name"], row["address"]
      hosts[name] ||= Hash.new
      hosts[name]["addresses"] ||= Array.new
      hosts[name]["addresses"] << address
      hosts[name]["image_name"] ||= "default"
    end
    node_roles.each do |nr|
      # Fix up the image for default
      nr_image = (Attrib.get("provisioner-docker-image", nr) rescue nil)
      nr_image ||= "ubuntu:14.04"
      hosts.keys.each do |name|
          if hosts[name]["image_name"] == "default"
            hosts[name]["image"] = nr_image
          else
            hosts[name]["image"] = hosts[name]["image_name"]
          end
      end

      nr.with_lock('FOR NO KEY UPDATE') do
        old_hosts = (nr.sysdata["rebar"]["docker"]["clients"] rescue {})
        next if old_hosts  == hosts
        nr.update_column("sysdata",{"rebar" => {"docker" => {"clients" => hosts}}})
        to_enqueue << nr
      end
    end
    to_enqueue.each { |nr| Run.enqueue(nr) }
  end
end
