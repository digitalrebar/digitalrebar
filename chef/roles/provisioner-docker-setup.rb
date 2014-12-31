name "provisioner-docker-setup"
description "Provisioner DHCP Database role"
run_list("recipe[provisioner::docker_setup]")
default_attributes()
override_attributes()
