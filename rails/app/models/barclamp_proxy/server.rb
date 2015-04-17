# Copyright 2015, Greg Althaus
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

class BarclampProxy::Server < Role

  def sysdata(nr)
    ranges = []
    NetworkRange.all.each do |r|
      next if r.first.v6?
      next if r.network.category == "unmanaged"

      ranges << r.first.network.to_s
    end

    {
      "crowbar" => {
        "proxy" => {
          "networks" => ranges
        }
      }
    }
  end

end
