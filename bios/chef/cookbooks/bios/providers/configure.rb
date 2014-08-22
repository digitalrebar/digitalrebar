# Copyright (c) 2013 Dell Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

require 'json'
require 'chef/shell_out'

def cnt_name(type)
  "bios_c_#{type}_attempts"
end

def get_count(type)
  c_name = cnt_name(type)
  node["crowbar_wall"] = {} unless node["crowbar_wall"]
  node["crowbar_wall"]["track"] = {} unless node["crowbar_wall"]["track"]
  node["crowbar_wall"]["track"][c_name] = 0 unless node["crowbar_wall"]["track"][c_name]
  node["crowbar_wall"]["track"][c_name]
end

def set_count(type, val)
  c_name = cnt_name(type)
  c = node["crowbar_wall"]["track"][c_name]
  node["crowbar_wall"]["track"][c_name] = val
  node.save
  return val
end

def up_count(type)
  c_name = cnt_name(type)
  c = node["crowbar_wall"]["track"][c_name] 
  node["crowbar_wall"]["track"][c_name] = c+1
  node.save
  return c+1
end

def can_try_again(type, max)
  count = get_count(type)
  Chef::Log.warn("Max allowed configure attempts : #{max}")
  try = (count < max)
  Chef::Log.warn("Attempts to configure #{type} so far: #{count} will #{ try ? "" : "not"}try again")  
  return try
end

#
# Return true if all products are up-to-date
# Returns false if we need to try again or on failure.
#
def wsman_configure(product, attrs)
  require 'wsman'
  # Get bmc parameters
  ip = node["crowbar_wall"]["ipmi"]["address"]
  user = node["ipmi"]["bmc_user"] rescue "crowbar"
  password = node["ipmi"]["bmc_password"] rescue "crowbar"

  opts = { :user => user, :password => password, :host => ip, :port => 443, :debug_time => false }
  wsman = Crowbar::WSMAN.new(opts)

  ## Seen issues on 11G where pending config jobs seem to be left behind...
  ## if updates go through all jobs are cleared out...else clear_all_jobs isn't called prior
  answer, status = wsman.clear_all_jobs
  if (answer)
    Chef::Log.info("Cleared all jobs...Sleeping for 1 mn")
    sleep 60
  else
    Chef::Log.info("Unable to clear jobs...May not be able to set attributes")
  end

  # Wait for RS ready
  local_count = 0
  begin
    local_count = local_count + 1

    ready, value = wsman.is_RS_ready?
    break if ready
    Chef::Log.info("WSMAN not ready configure attributes: #{value}")
    sleep 30
  end while local_count < 4
  if local_count == 4
    Chef::Log.info("WSMAN not ready configure attributes, return fail")
    return false
  end

  # Do the attr updates
  wsman_attributes = Crowbar::BIOS::WSMANAttributes.new(wsman)
  opts = { :node => node }
  ret, reboot = wsman_attributes.update_attributes(attrs, opts)
  Chef::Log.info("WSMAN update attributes: #{ret} #{reboot}")
  report_problem(reboot) unless ret
  node.save # Just in case the update attributes changed something
  %x{reboot && sleep 120} if ret and reboot
  return ret
end


##
# log to the problem file.
#
#
def report_problem(msg)
  problem_file = @new_resource.problem_file
  puts("BIOS:WSMAN reporting problem to: #{problem_file}- #{msg}" )
  unless problem_file.nil?
    open(problem_file,"a") { |f| f.puts(msg) }
  end
  puts(msg)
end

action :configure do
  product = @new_resource.product
  type = @new_resource.type
  max_tries = @new_resource.max_tries
  values = @new_resource.values

  if type == "wsman"
    begin
      break unless can_try_again(type, max_tries)
      up_count(type)
      if wsman_configure(product, values)
        # Clear count on success
        set_count(type, 0)
        break
      end
    end while false
  end
end

