name "dhcp-database"
description "DHCP Database role"
run_list("recipe[dhcp]","recipe[dhcp::update_nodes]")
default_attributes()
override_attributes()
