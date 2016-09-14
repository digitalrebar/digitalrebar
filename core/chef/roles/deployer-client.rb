name "deployer-client"
description "Deployer Client role - Discovery components"
run_list(
         "recipe[barclamp]",
         "recipe[rebar-hacks]",
         "recipe[ohai]",
         "recipe[kernel-panic]"
)
default_attributes()
override_attributes()

