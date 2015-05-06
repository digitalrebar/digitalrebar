name "dns-bind_database"
description "DNS Database Role - DNS server for the cloud"
run_list(
         "recipe[bind9::database]"
)
