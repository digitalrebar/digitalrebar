name             'network'
maintainer       "Dell Crowbar Team"
maintainer_email "openstack@dell.com"
license          "Apache 2"
description      "Network configuration and management for Chef and Crowbar"
long_description "Network configuration and management for Chef and Crowbar"
version          "0.9.5"
recipe "network::default", "Manages networks"
recipe "network::lldpd", "Installs LLDPD service"
