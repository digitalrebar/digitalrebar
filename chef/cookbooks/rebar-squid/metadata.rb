name             'rebar-squid'
maintainer       "DigitalRebar Team"
maintainer_email "victor.lowther@gmail.com"
license          "Apache 2.0"
description      "Installs squid for Rebar"
long_description "Installs squid for Rebar"
version          "0.2"
recipe "rebar-squid::server", "Handles setting up a squid proxy"
recipe "rebar-squid::client", "Handles setting up a system to use a squid proxy"

