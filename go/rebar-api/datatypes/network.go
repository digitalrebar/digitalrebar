package datatypes

import "github.com/guregu/null"

// Network holds configuration for a specific network.
type Network struct {
	NameID
	// DeploymentID is the deployment for which this network is defined.
	DeploymentID int64 `json:"deployment_id"`
	// Vlan is the VLAN that this network should run over.
	Vlan int64 `json:"vlan"`
	// TeamingMode is the bonding mode that the conduit should use.
	TeamingMode int64 `json:"team_mode"`
	// UseTeam controls whether the Network should operate in teamed mode.
	UseTeam bool `json:"use_team"`
	// UseVlan controles whether the network should run over a specific
	// tagged VLAN interface.
	UseVlan bool `json:"use_vlan"`
	// UseBridge controls whether this Network should allocate a bridge layer.
	// This can be useful if you know that the workload this network will provide
	// services for will require attaching virtual interfaces to this interface.
	UseBridge bool `json:"use_bridge"`
	Configure bool `json:"configure"`
	// The description of this network.
	Description string `json:"description"`
	// The IPv6 prefix this network should use to hand out IPv6 addresses.
	// If empty, no IPv6 addresses will be allocated.  If the address is not in CIDR
	// form, Rebar will assume it is a /64 subnet.
	V6Prefix null.String `json:"v6prefix"`
	// The conduit that this Network should run over.  A Conduit
	// defnintion is a comma-seperated list of abstract interface
	// names that have the following format:
	//
	//    <sign><speed><#> where
	//
	//    * sign is optional, and determines behavior if exact
	//      match is not found.
	//      + allows speed upgrade,
	//      - allows downgrade, and
	//      ? allows either.
	//
	//      If no sign is specified, an exact match must be found.
	//
	//    * speed designates the interface speed. 10m, 100m, 1g
	//    and 10g are supported
	//
	//    * The final number designates the zero-based offset into
	//    the set of physical interfaces that have the requested
	//    speed we want.
	//
	//   Multiple networks are allowed to run over the same
	//   Conduit, and Conduit specifications for different
	//   networks on the same machine must either overlap
	//   perfectly or not at all.
	Conduit          string      `json:"conduit"`
	Category         string      `json:"category"`
	Group            string      `json:"group"`
	PolicyBasedRoute null.String `json:"pbr"`
	TenantID         int64       `json:"tenant_id,omitempty"`
}

func (o *Network) ApiName() string {
	return "networks"
}

// NetworkRange defines a specific range of allocatable addresses in a
// Network.  All address allocation for a Node must come from one
// specific Range per Network.  NetworkRanges are allowed to have
// different teaming, VLAN, bonding, and conduit specifications than
// their parent Network.
type NetworkRange struct {
	SimpleID
	NetworkID   int64       `json:"network_id"`
	Vlan        null.Int    `json:"vlan"`
	TeamingMode null.Int    `json:"team_mode"`
	UseTeam     null.Bool   `json:"use_team"`
	UseVlan     null.Bool   `json:"use_vlan"`
	UseBridge   null.Bool   `json:"use_bridge"`
	Overlap     bool        `json:"overlap"`
	Conduit     null.String `json:"conduit"`
	// The first address that can be allocated in this NetworkRange.
	// The address can be either IPv4 or IPv6, and must be in CIDR form.
	First string `json:"first"`
	// The last address that can be callocated from this NetworkRange.
	// It must be of the same type and in the same subnet as the First address.
	Last             string `json:"last"`
	Name             string `json:"name"`
	TenantID         int64  `json:"tenant_id,omitempty"`
	AnonLeaseTime    int64  `json:"anon_lease_time"`
	BoundLeaseTime   int64  `json:"bound_lease_time"`
	AllowAnonLeases  bool   `json:"allow_anon_leases"`
	AllowBoundLeases bool   `json:"allow_bound_leases"`
}

func (o *NetworkRange) ApiName() string {
	return "network_ranges"
}

// AddressID is used for NetworkAllocation and NetworkRouter, as they
// can be uniquely identified by either their ID or Address fields.
type AddressID struct {
	SimpleID
	// Address is an IPv4 or v6 address in CIDR format.
	Address string `json:"address"`
}

// Id returns this attrib's ID or Name as a string.
// The REST API allows them to be used interchangeably.
func (o *AddressID) Id() (string, error) {
	if o.Address != "" {
		return o.Address, nil
	}
	return o.SimpleID.Id()
}

// SetId sets either the ID or the Name field, depending on whether
// the passed-in string can be parsed as an int64 or not.
func (o *AddressID) SetId(s string) error {
	err := o.SimpleID.SetId(s)
	if err == nil {
		return nil
	} else if err == SetIDErr {
		return err
	}
	o.Address = s
	return nil
}

// NetworkAllocation is the allocation of an address from a NetworkRange to a Node.
type NetworkAllocation struct {
	AddressID
	NodeID         null.Int `json:"node_id"`
	NetworkID      null.Int `json:"network_id"`
	NetworkRangeID null.Int `json:"network_range_id"`
	TenantID       int64    `json:"tenant_id,omitempty"`
}

func (o *NetworkAllocation) ApiName() string {
	return "network_allocations"
}

// NetworkRouter holds the address that should be used to route
// traffic out of the network it is associated with.
type NetworkRouter struct {
	AddressID
	NetworkID int64 `json:"network_id"`
	TenantID  int64 `json:"tenant_id,omitempty"`
	Pref      int64 `json:"pref"`
}

func (o *NetworkRouter) ApiName() string {
	return "network_routers"
}
