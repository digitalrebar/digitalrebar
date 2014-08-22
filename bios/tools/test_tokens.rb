#! /usr/bin/env ruby
# Copyright 2011, Dell
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

=begin
This utility read the set of available tokens on the machine it's running on, and then tests the value of each (set/unset).

It is intended to "discover" the correct Token for a given BIOS setting. 
The procude is:
- set the BIOS value to a setting (via GUI)
- login, and dump the current tokens to a file (before)
- reboot
- set the BIOS value to a different setting (via GUI)
- loing and dump the settings to (after)
- diff the 2 files to see what changed.


=end
t_lst = %x{/usr/sbin/setupbios list_tokens}
tokens = []
t_lst.each { |x|
  sp = x.split()
  tokens << sp[6]
}

tokens.each { |t| 
  cmd = "/usr/sbin/setupbios test #{t}"
  val = %x{ #{cmd} }
  puts "t: #{t} #{val}"  
}





