## DNS Name Filter

The DNS Name Filter is used to watch the Network Subsystem and generate DNS names for interface on nodes.  These names are sent
to the DNS Management Services to update the DNS systems.

Each filter has 4 components and an optional *name*.  The *name* is a helpful identifier to find or provide meaning to the filter.

The first mandatory component is *system*.  This is a string that matches the name of a DNS Management service.
The DNS management service will be looked up and its API called to add or remove Name/IP(v6) pairs.

The second mandatory component is *prioirty*. This is an integer value that must be unique amoung all filters.  This defines an order
from lowest to highest integer value.  The filters are evaluated in this order.  The first matching filter is applied to the 
network allocation in question.

The third mandatory component is *matcher*.  This is a string that defines a matching string that is decoded and 
used to match network allocations.

The following substrings are allowed joined by commas.  The quotes around the right side of == are required.
* net.name == "*admin*" - if the network allocation is from the *admin* network, consider matched
* net.category == "*internal*" - if the network allocation is from a network with category *internal*, consider matched
* range.name == "*host*" - if the network allocation is from the *host* range, consider matched.
* deployment.name == "*system*" - the node is in the *system* deployment, consider matched.
* node.role has "*role*" - if the node has the *role* assigned to it, consider matched.
* node.attr.*attr* == "*value*" - if the node has the *attr* and it has the value of *value*, consider matched.

*admin*, *internal*, *host*, *system*, *role*, *attr*, and *value* can be replaced with custom values.  *attr* is the barclamp name
for the attribute (not the map name).  *value* must be the string form of the contents of that attribute.

If more than one substring is provided, all substrings *must* match.

The forth mandatory component is *template*.  This string defines a template when used with other internal structures generates
an FQDN for this network allocation.

The template is a string that is an FQDN with the following strings that can be replaced.
* {{node.name}} - the current name of the noade truncated at the first '.'
* {{node.id}} - The id of the node in integer form
* {{node.mac}} - The mac address of the node with the '.' or ':' replaced with '-'

The system will build matches for both IPv4 and IPv6 allocations.

## DNS Name Entry

This model contains a helper set of references to quickly generate requests to the DNS Management Services.

The model contains a reference to a NetworkAllocation that has matched a specific DnsNameFilter that has matched this allocation.
The rendered name and the record type are record to assist in external request generation.

The API and CLI for these only really support GET operations.