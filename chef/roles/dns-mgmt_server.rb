name "dns-mgmt_server"
description "DNS Management Server Role"
run_list(
         "recipe[crowbar::dns_mgmt_server]"
)
