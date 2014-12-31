name "dns-database"
description "DNS Database Role - DNS server for the cloud"
run_list(
         "recipe[bind9::database]"
)
