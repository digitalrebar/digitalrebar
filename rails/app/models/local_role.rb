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

class LocalRole < Role

  def run_local(cmd, nr = nil)
    Rails.logger.debug("LocalRole: #{self.name}: Running #{cmd}")
    out,err = '',''
    status = Open4::popen4ext(true,cmd) do |pid,stdin,stdout,stderr|
      stdin.close
      begin
        # Readpartial will read all the data up to 16K
        # If not data is present, it will block
        loop do
          out << stdout.readpartial(16384)
          nr.update!(runlog: out) if nr
        end
      rescue Errno::EAGAIN
        retry
      rescue EOFError
      end

      err << stderr.read
      stdout.close
      stderr.close
    end
    [out,err,status]
  end

end

