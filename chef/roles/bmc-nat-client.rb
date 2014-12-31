name "bmc-nat-client"
description "Sets up routes to access BMC addresses"
run_list(
         "recipe[bmc-nat::client]"
)
default_attributes()
override_attributes()
