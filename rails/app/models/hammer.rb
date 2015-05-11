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

# Hammers encapsulate the concept of being able to
# perform actions on a node outside the context of the noderole
# graph.  Exmaples of these actions include:
# * Copying files to and from the system.
# * Executing a command on the system as someone with
#   admin rights.
# * Powering a system up or down.
# * Changing the boot target of a system.
# * Any other node management task that does not belong on the
#   node role graph.

class Hammer < ActiveRecord::Base

  audited

  belongs_to :node
  belongs_to :available_hammer

  def self.bind(args)
    raise "Must pass a manager_name: arg" unless args[:manager_name]
    Rails.logger.info("Hammer: Binding #{args[:manager_name]} to #{args[:node].name}")
    anm = AvailableHammer.find_by!(name: args.delete(:manager_name))
    Rails.logger.debug("Hammer: #{args.inspect}")
    args[:available_hammer] = anm
    args[:name] = anm.name
    args[:type] = anm.klass
    args[:priority] ||= anm.priority
    Hammer.create!(args)
  end

  def as_json(args)
    args[:methods] = :actions
    super(args)
  end

  def actions
    {}
  end

  def self.probe(node)
    false
  end

  def self.gather(node)
    res = Hash.new
    node.hammers.order("priority DESC").each do |mgr|
      mgr.actions.each do |k,v|
        res[k] ||= Hash.new
        v.each do |meth|
          next if res[k][meth]
          res[k].define_singleton_method(meth) do |*args|
            mgr.send(meth,*args)
          end
          res[k][meth] = res[k].method(meth)
        end
      end
    end
    res
  end
end
