name "consul"
description "Consul provides a distributed, fault-tolerant service discovery and monitoring solution."
run_list(
         "recipe[consul]",
         "recipe[utils]",
         "recipe[barclamp]"
)
