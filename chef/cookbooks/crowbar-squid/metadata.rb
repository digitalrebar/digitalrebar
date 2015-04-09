name             'crowbar-squid'
maintainer       "OpenCrowbar Team"
maintainer_email "victor.lowther@gmail.com"
license          "Apache 2.0"
description      "Installs squid for Crowbar"
long_description "Installs squid for Crowbar"
version          "0.2"
recipe "crowbar-squid::server", "Handles setting up a squid proxy"
recipe "crowbar-squid::client", "Handles setting up a system to use a squid proxy"

