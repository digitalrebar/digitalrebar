#!/bin/bash
# Copyright 2014, Dell
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

echo "REMINDER: you must be running a Crowbar server to run these tests"

# we need erlang!
if [[ ! -e /usr/bin/erl ]]; then
  # todo, make this multi OS
  sudo apt-get -y install erlang-base erlang-inets
fi

# $1 = change the config file.  Default is "default".  use "production" for live sites

cd BDD


# we need the config file!
if [[ ! -f default.confg ]]; then
  echo "using reference config, please review and update"
  cp example.config default.config
  cat default.config
fi


# Clean-up
rm -f ../erl_crash.dump /tmp/trace_*.txt

# Compile
if [[ ! -f bdd.beam  ]]; then
  echo "compiling"
  erlc +debug_info *.erl
else
  echo "using compiled code - this may skip changes to BDD erlang code.  Use tools/test_cleanup.sh to recompile"
fi

erl -s bdd test $1 -s init stop -noshell
