# Copyright 2015, RackN
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

class BarclampTest::Async < Role

  def do_transition(nr, data)

    runlog = []
    service_name = 'async-result'
    file_name = File.join 'tmp', "async_#{nr.id}.txt"
    
    runlog << "Processing pieces for #{service_name}"

    # CASE CLOSE - normal close
    if File.exists? file_name
      # read the file
      ticket = JSON::load File.open(file_name, 'r')
      # set the attributes
      runlog << "Setting #{service_name} attribute"
      Attrib.set('async-result', nr, ticket, :system)
      raise "#{service_name} not available" if false  # test to make sure we're ok
      # put data into the run log
      runlog << "Finished processing pieces for #{service_name}"
      nr.runlog = runlog.join("\n")
      nr.save!
      # finish up
      Rails.logger.info("Finished waiting for #{service_name}")
      return nil      
    end

    # CASE ASYNC - keep running
    # create the file
    File.open(file_name,"w") { |f| f.write("{\"async\":{\"result\":#{nr.id}}}") }
    Rails.logger.info("Started waiting for #{service_name}.  Created file #{file_name}")
      # put data into the run log
    runlog << "Processing pieces for #{service_name}"
    nr.runlog = runlog.join("\n")
    nr.save!
    # this will allow run to complete but role stays in execute
    return :async

  end

end
