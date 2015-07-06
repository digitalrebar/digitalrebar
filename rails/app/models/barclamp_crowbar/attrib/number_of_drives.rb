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

class BarclampCrowbar::Attrib::NumberOfDrives < Attrib

  # number_of_drives has to calculate info from the data, not just look it up
  def get(data,source=:all, committed=false)

    # we want to leverage the map for the data in general
    # just we only want to sd* drives
    # and only use discovered data.
    val = super(data, :discovery, committed)
    sd = val.select { |k, v| k =~ /sd([a-z])/ } rescue {}
    # now that we have the list, return the fount
    return sd.length rescue -1

  end

end
