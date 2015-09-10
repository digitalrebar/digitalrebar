name             'network'
maintainer       "Rebar Team"
maintainer_email "support@rackn.com"
license          "Apache 2"
description      "Network configuration and management for Chef and Rebar"
long_description "Network configuration and management for Chef and Rebar"
version          "0.9.5"
recipe "network::default", "Manages networks"
recipe "network::lldpd", "Installs LLDPD service"
