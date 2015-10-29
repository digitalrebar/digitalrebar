#!/usr/bin/env bash
# Copyright 2015, RackN
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License
# You may obtain a copy of the License at
#
#  http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied
# See the License for the specific language governing permissions and
# limitations under the License
#

proxy_str="$(read_attribute "rebar/proxy/servers/0/url")"
proxy_addr="$(read_attribute "rebar/proxy/servers/0/address")"

no_proxy="127.0.0.1,localhost,::1,/var/run/docker.sock,$proxy_addr"
# Read json array of admin addresses
admin_addrs=$(read_attribute "proxy/admin_addrs")
if [[ $admin_addrs && $admin_addrs != '[]' ]] ; then
  admin_addrs=$(echo "$admin_addrs" | jq -r 'map(. + ",") |add')
  no_proxy="$admin_addrs,$no_proxy"
fi
# Process ip -o addr show for IP addresses
while read line; do
    no_proxy="$no_proxy,${line%/*}"
done < <(ip -4 -o addr show | awk '{ print $4 }')

# look for environment variable
if [[ ! $proxy_str ]] ; then
  proxy_str=$http_proxy
fi

# if we have an internet IP address, then don't use the proxy.
# This is cheesy, but functional for now.
addrs=$(ip -o -4 addr show scope global |awk '!/ (10|192\.168|172\.(2[0-9]|1[6-9]|3[0-1]))\./ {print $4}')
if [[ $addrs ]] ; then
  echo "Internet addresses only - no proxy"
  exit 0
fi


# Nothing to do
if [[ ! $proxy_str ]] ; then
  exit 0
fi

cat >>/etc/environment <<EOF
http_proxy=$proxy_str
https_proxy=$proxy_str
no_proxy=$no_proxy
EOF

cat >/etc/profile.d/proxy.sh <<EOF
export http_proxy=$proxy_str
export https_proxy=$proxy_str
export no_proxy=$no_proxy
EOF

if [ -e /etc/yum.repos.d ] ; then
  if [ -e /etc/os-release ] ; then
    . /etc/os-release
  else 
    if [ -e /etc/system-release-cpe ] ; then
      ID=$(cat /etc/system-release-cpe | awk -F: '{ print $3 }')
    fi
  fi
  cat >/etc/yum.conf <<EOF
[main]
cachedir=/var/cache/yum/$basearch/$releasever
keepcache=0
debuglevel=2
logfile=/var/log/yum.log
exactarch=1
obsoletes=1
gpgcheck=1
plugins=1
installonly_limit=5
#bugtracker_url=http://bugs.centos.org/set_project.php?project_id=16&ref=http://bugs.centos.org/bug_report_page.php?category=yum
distroverpkg=$ID-release
proxy=$proxy_str
EOF
fi

