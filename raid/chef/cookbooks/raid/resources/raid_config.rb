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

=begin
  Configure the available drives for RAID
  Sample use:

  raid_config "lsi_ircu" do
    config( 
       { "vol-raid1" => {:raid_level => :RAID1}, ## must use exactly 2 disks
         "vol-raid0" => {:raid_level => :RAID0, :disks = 3 },
         "default" =>  {:raid_level => :JBOD }
     action [:apply, :report]
  end
 
=end

## what to do:
###  apply - configure the RAID controller
###  report - update node with info about the physical disks
###  set_boot - update the controller's boot volume/disk
actions :apply, :report, :set_boot
## what raind configuration to apply - hash, see above
attribute :config
## should really verbose output be produced - true/false
attribute :debug_flag 
## where to log problems encountered - string
attribute :problem_file
## Boot parameter that indicates if nic should be first
attribute :nic_first


