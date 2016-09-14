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

RAID_DEPS=("8.07.07_MegaCLI.zip" "SAS2IRCU_P16.zip")

# This has been expanded to not only check for the exisitence of the files
# that are needed in the cache, but to attempt to pull them in from $HOME
# if they are not in the cache

bc_needs_build() {
    rc=1
    for f in "${RAID_DEPS[@]}"; do
        if [ ! -f $BC_CACHE/files/raid/tools/$f ] ; then # check if the file is there
            if [ -f $HOME/$f ]; then                          # if not, check in $HOME
                mkdir -p $BC_CACHE/files/raid/tools
                cp $HOME/$f $BC_CACHE/files/raid/tools/  #grab it
	    else
                rc=0 # fail if it isn't there
            fi
        fi
    done
    return $rc
}

bc_build() {
    exec >&2
    echo "Please download:"
    echo "http://www.lsi.com/downloads/Public/Host%20Bus%20Adapters/Host%20Bus%20Adapters%20Common%20Files/SAS_SATA_6G_P16/SAS2IRCU_P16.zip"
    echo "http://www.lsi.com/downloads/Public/MegaRAID%20Common%20Files/8.07.07_MegaCLI.zip"
    echo
    echo "into $BC_CACHE/files/raid/tools/ or your home directory"
    return 1
}
