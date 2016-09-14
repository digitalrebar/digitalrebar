name "bmc-nat-router"
description "Configures a node to nat to the BMC network"
run_list(
         "recipe[bmc-nat::router]"
)
default_attributes()
override_attributes()
