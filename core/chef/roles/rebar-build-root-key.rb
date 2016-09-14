
name "rebar-build-root-key"
description "Build a root key if needed and record the public key"
run_list(
         "recipe[utils]",
         "recipe[provisioner::make_ssh_keys]"
)
default_attributes()
override_attributes()
