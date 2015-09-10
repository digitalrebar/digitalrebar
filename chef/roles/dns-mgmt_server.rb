name "dns-mgmt_server"
description "DNS Management Server Role"
run_list(
         "recipe[rebar::dns_mgmt_server]"
)
