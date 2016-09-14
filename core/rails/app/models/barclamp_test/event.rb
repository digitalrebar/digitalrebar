# Copyright 2013, Dell 
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

class BarclampTest::Event < Role


  def on_error(node_role, *args)
    Rails.logger.info "TEST JIG >>>> Firing on_error for #{node_role.role.name} on #{node_role.node.name} <<<< GIJ TSET"
    puts "TEST JIG >>>> Firing on_error for #{node_role.role.name} on #{node_role.node.name} <<<< GIJ TSET"
  end

  def on_active(node_role, *args)
    Rails.logger.info "TEST JIG >>>> Firing on_active for #{node_role.role.name} on #{node_role.node.name} <<<< GIJ TSET"
    puts "TEST JIG >>>> Firing on_active for #{node_role.role.name} on #{node_role.node.name} <<<< GIJ TSET"
  end

  def on_todo(node_role, *args)
    Rails.logger.info "TEST JIG >>>> Firing on_todo for #{node_role.role.name} on #{node_role.node.name} <<<< GIJ TSET"
    puts "TEST JIG >>>> Firing on_todo for #{node_role.role.name} on #{node_role.node.name} <<<< GIJ TSET"
  end

  def on_transition(node_role, *args)
    Rails.logger.info "TEST JIG >>>> Firing on_transition for #{node_role.role.name} on #{node_role.node.name} <<<< GIJ TSET"
    puts "TEST JIG >>>> Firing on_transition for #{node_role.role.name} on #{node_role.node.name} <<<< GIJ TSET"
  end

  def on_blocked(node_role, *args)
    Rails.logger.info "TEST JIG >>>> Firing on_blocked for #{node_role.role.name} on #{node_role.node.name} <<<< GIJ TSET"
    puts "TEST JIG >>>> Firing on_blocked for #{node_role.role.name} on #{node_role.node.name} <<<< GIJ TSET"
  end

  def on_proposed(node_role, *args)
    Rails.logger.info "TEST JIG >>>> Firing on_proposed for #{node_role.role.name} on #{node_role.node.name} <<<< GIJ TSET"
    puts "TEST JIG >>>> Firing on_proposed for #{node_role.role.name} on #{node_role.node.name} <<<< GIJ TSET"
  end

end

