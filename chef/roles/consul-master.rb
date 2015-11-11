name "consul-master"
description "Consul provides a distributed, fault-tolerant service discovery and monitoring solution."
run_list(
         "recipe[utils]",
         "recipe[barclamp]",
         "recipe[consul::install]",
         "recipe[consul::ui]",
         "recipe[consul::start-master]",
         "recipe[consul::start-service]"
)
