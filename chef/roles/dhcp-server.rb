name "dhcp-server"
description "DHCP Server role"
run_list("recipe[dhcp::install]","recipe[dhcp::default]")
default_attributes()
override_attributes()
