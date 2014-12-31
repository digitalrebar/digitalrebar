## Network Model

Crowbar orchestrations the creation of networks and connects them to other roles using conduits

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