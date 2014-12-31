
name "provisioner-base-images"
description "Provisioner Server role - Apt and Networking"
run_list(
         "recipe[utils]",
         "recipe[provisioner::setup_base_images]"
)
default_attributes()
override_attributes()
