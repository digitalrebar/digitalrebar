name "consul-member"
description "Consul provides a distributed, fault-tolerant service discovery and monitoring solution."
run_list(
         "recipe[utils]",
         "recipe[barclamp]",
         "recipe[consul::install]",
         "recipe[consul::start-member]",
         "recipe[consul::start-service]"
)
