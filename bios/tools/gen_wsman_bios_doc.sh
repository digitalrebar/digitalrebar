#!/bin/bash
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


########
# this script looks for bios setting files in the current directory, and produces a more human 
# friendly rendition of its content.


for plat in R610 R620 R710 R720 R720xd; do
  for file in `ls *${plat}-*`; do 
     egrep  "(value|attr_name)" $file | awk '
/attr_name/ { name=$2; n=1; next}  /value/ {value=$2; v=1  } v==1 && n==1 { print name "= " value
; n=0; v=0} ' > ${plat}_${file##*$plat}.txt
  done
done

