name "provisioner-database"
description "Provisioner Database role"
run_list("recipe[provisioner::update_nodes]")
default_attributes()
override_attributes()
