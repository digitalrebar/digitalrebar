name "dns-bind_server"
description "DNS Server Role - BIND DNS server for the cloud"
run_list(
         "recipe[bind9::install]","recipe[bind9::default]"
)
