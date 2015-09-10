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

[[ $BC_CACHE ]] || export BC_CACHE="$HOME/.rebar-build-cache/barclamps/bios"
[[ $REBAR_DIR ]] || export REBAR_DIR="$HOME/rebar"
[[ $BC_DIR ]] || export BC_DIR="$HOME/rebar/barclamps/bios"

echo "Using: BC_CACHE = $BC_CACHE"
echo "Using: REBAR_DIR = $REBAR_DIR"
echo "Using: BC_DIR = $BC_DIR"

bc_needs_build() {
  [[ ! -f "$BC_CACHE/files/wsman/Catalog.xml" ]]
}

bc_build() {
    mkdir -p "$BC_CACHE/files/wsman"

    cd "$BC_CACHE/files/wsman"
    curl -s http://ftp.us.dell.com/catalog/Catalog.cab > Catalog.cab
    cabextract Catalog.cab
    rm -rf Catalog.cab
    [[ ! -f Catalog.xml ]] && die "Can't find Catalog.xml"
    iconv --from-code UCS-2 --to-code UTF-8 Catalog.xml | sed "s/utf-16/utf-8/" > Catalog.xml.fix
    mv Catalog.xml.fix Catalog.xml
    cd -

    "$BC_DIR/extract_packages.rb" "$BC_DIR/rebar.yml" "$BC_CACHE/files/wsman"
}

