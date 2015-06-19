# DNS Subsystem

The DNS Subsystem consists of three components: the server, management server, and filters.

## DNS Server and Service

The DNS server can be provided by the OpenCrowbar system or externally.  Currently, the OpenCrowbar DNS server
is BIND.  The BIND server will set up everything needed run a DNS server.  External DNS server can be anything, but
needs to be injected into the consul services.  This is done in crowbar-config.sh.  See that file for how to select
a server.

## DNS Management Server and Service

The management server is used to inject information into the DNS server.  The management server runs on the same system as the
BIND DNS server and rebuilds files.  The management server can also manage an PowerDNS server through its HTTP API (version 3.4.5
or higher).  The management server can be on any system in that case.  Currently, only external PowerDNS servers can be
managed remotely.  The management server needs some additional information like access token and a few other things.  These are
provided as consul key/value pairs associated with the external PowerDNS service injected into consul. These are also 
specified in crowbar-config.sh along with external service registration.

## DNS Name Filters

The third component of the DNS subsystem are filters.  The Name filters provide a mechanism to generate names for address 
allocations on nodes.  These name/IP pairs are sent to a specified DNS management service to handle updating the DNS server.
Filters are ordered and evaluated until matched.  Once matched, a name is generate and the service is called.  A template
system is used to generate names from node attributes.
