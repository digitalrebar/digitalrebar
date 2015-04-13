name "proxy-server"
description "Proxy Server Role - Proxy server for the cloud (squid)"
run_list(
         "recipe[crowbar-squid::server]"
)
