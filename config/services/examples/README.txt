
The general form of services is:
{
  "type": "internal" or "external",
  "name": "name of service",
Internal variables -
  "has_service": true,
  "server_role": "ntp-server",
  "service_role": "ntp-service",
  "attributes": {
     "attrname": "value"
  }
External variables -
  "datacenter": "digitalrebar",
  "node_name": "external",
  "service_ip": "1.1.1.1",
  "service_port": 3535,
  "service_tag": "system",
  "keys": {
     "key": "value",
  }
}


type/internal - means that open rebar will run a server of this name. 
type/external - means that open rebar will add the service to consul.

name should have one of the following values.  Additional values will
attempt to be built from a set of rules.
Known services:
  dhcp 
  ntp
  dns
  dns-mgmt
  amqp
  proxy
  provisioner
  rebar-api
  rebar-job_runner

has-service is a boolean value that indicates if a service should be
started for the server.  This defaults to true.

server-role specifies the OCB role that provides the server.  This can
be derived or overridden by this value.  Multiple can be specified as 
comma separated list.

service-role specifies the OCB role that provides the service.  This can
be derived or overridden by this value.  Multiple can be specified as a
comma seperated list.

An optional hash of attributes can be specified to set variables after
the roles have been added.  The JSON subsection will be passed directly.

An unspecified service will have to following rules applied.
  service - <name>-service unless name has a '-', then <name>_service
  server - <name>-server unless name has a '-', then <name>_server

Add 01-XX in front of the service file name to get in the list different.
