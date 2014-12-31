name "dns-server"
description "DNS Server Role - DNS server for the cloud"
run_list(
         "recipe[bind9::install]","recipe[bind9::default]"
)
