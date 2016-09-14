# Digital Rebar Forwarder

This is a IP Tables based forwarder.  It allows the container to forward
ports discovered in consul as if it came from its own IP.  The IP is
specified by the FORWARDER_IP environment variable injected into the
container.

Services named internal-ZZZ will be exported as ZZZ as a direct one to
one port mapping.  Tags will be copied over.

