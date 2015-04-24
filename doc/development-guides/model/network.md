## Network Model

Crowbar orchestrations the creation of networks and connects them to other roles using conduits

### Categories

Networks have a category.  The category is used to classify the network type.  This is a string
field and can be anything.  There are some special values for OpenCrowbar.

  * admin - This indicates that the network is an admin network.  Admin networks are preferred for communication paths for
  bring-up and installion.  When a node is discovered, an admin address is assigned from the networks marked *admin*.  If
  an *admin* network is not found, the hinted address will be allocated from the *unmanaged* network.
  * bmc - This indcates that the network is a BMC network.  The BMC-type network is used by the IPMI configuration subsystem
  to allocate an address for the BMC.
  
### Groups

Networks have a group.  The group is used to cluster networks together.  In general, this is just 
for identification with the exception of BMC networks.  When the IPMI subsystem needs to allocate a
BMC address, the node will be searched for other networks.  Once these networks are found, the groups of
these networks will be used to find a BMC network that could be used for allocation.

### Conduits

A logical description of the networks to help Crowbar bind generically across hardware platforms. 

On physical infrastructure, NIC enumeration is unpredictable and not helpful for functional 
topologies.  For example, a storage application wants to bind it's primary service interface 
to the fasted teamed network available; however, there is no default teamed network because 
they have to be constructed.  

Crowbar uses the network and conduit information to build abstract networks for use by other
roles.

Conduits are defined using a Speed + Order syntax.  Speed are given as 1g, 10g, 100m, etc
and then order is ordinal from 0.  The first 10g network would be noted as 10g0.

> The on board management (aka IPMI or BMC) interface is a special conduit named 'bmc'

Please import more reference more material!  There is a lot written about conduits for CB1.


### IPv6 support.  

You can create ranges and routers for IPv6 addresses
  as well as IPv4 addresses, and you can tell a network that it should
  automatically assign IPv6 addresses to every node on that network by
  setting the v6prefix setting for that network to either:
  
  * a /64 network prefix, or
  * "auto", which will create a globally unique RFC4193 IPv6 network
    prefix from a randomly-chosen 40 bit number (unique per cluster
    installation) followed by a subnet ID based on the ID of the
    OpenCrowbar network.

  Either way, nodes in a OpenCrowbar network that has a v6prefix will get
  an interface ID that maps back to their FQDN via the last 64 bits of
  the md5sum of that FQDN. For now, the admin network will
  automatically create an RFC4193 IPv6 network if it is not passed a
  v6prefix so that we can easily test all the core OpenCrowbar components
  with IPv6 as well as IPv4.  The DNS barclamp has been updated to
  create the appropriate AAAA records for any IPv6 addresses in the
  admin network.

### Ranges (networks_ranges)

A network has at least 1 range.  Ranges are use to allocate v4 and v6 IP addresses to nodes 
using the network.

Ranges can have distinct conduits but will share the network conduit by default.

### Router (networks_router)

Optional.

Describes the router used by the network and is required if the operator wishes to have Crowbar configure a router when the network is setup on a node.

### Allocations (networks_allcation)

Tracks the IP addresses that have been assigned to the network.

IPs are allocated as CIDRs.
