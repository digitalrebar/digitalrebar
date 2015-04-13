
name "provisioner-server"
description "Provisioner Server role - Apt and Networking"
run_list(
         "recipe[utils]",
         "recipe[provisioner::make_ssh_keys]",
         "recipe[provisioner::servers]"
)
default_attributes()
override_attributes()
