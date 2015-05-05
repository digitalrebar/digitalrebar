
name "rabbitmq-server"
description "RabbitMQ Servier Role"
run_list(
  "recipe[rabbitmq::cluster]",
  "recipe[rabbitmq::community_plugins]",
  "recipe[rabbitmq::mgmt_console]",
  "recipe[rabbitmq::plugin_management]",
  "recipe[rabbitmq::policy_management]",
  "recipe[rabbitmq::user_management]",
  "recipe[rabbitmq::virtualhost_management]",
  "recipe[rabbitmq::consul]"
)
default_attributes()
override_attributes()

