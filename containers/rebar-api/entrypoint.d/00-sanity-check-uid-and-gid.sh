#!/bin/bash

[[ -d /opt/digitalrebar/core ]] || exit 1
uid="$(stat -c '%u' /opt/digitalrebar/core)"
gid="$(stat -c '%g' /opt/digitalrebar/core)"

find /var /home -xdev -user rebar -exec chown "$uid:$gid" '{}' ';'
find /var /home -xdev -group rebar -exec chown "$uid:$gid" '{}' ';'
usermod -o -u "$uid" rebar
groupmod -o -g "$gid" rebar
