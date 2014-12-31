
name "network-lldpd"
description "Network LLDPD Role - Run the lldpd daemon"
run_list(
         "recipe[network::lldpd]"
)
default_attributes()
override_attributes()

