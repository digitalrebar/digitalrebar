package crowbar

import (
	"errors"
	"fmt"
	"log"
	"strconv"
)

// Network holds configuration for a specific network.
type Network struct {
	// ID is an opaque integer used to uniquely identify the network.
	ID int64 `json:"id,omitempty"`
	// DeploymentID is the deployment for which this network is defined.
	DeploymentID int64 `json:"deployment_id,omitempty"`
	// Vlan is the VLAN that this network should run over.
	Vlan int64 `json:"vlan,omitempty"`
	// TeamingMode is the bonding mode that the conduit should use.
	TeamingMode int64 `json:"team_mode,omitempty"`
	// UseTeam controls whether the Network should operate in teamed mode.
	UseTeam bool `json:"use_team,omitempty"`
	// UseVlan controles whether the network should run over a specific
	// tagged VLAN interface.
	UseVlan bool `json:"use_vlan,omitempty"`
	// UseBridge controls whether this Network should allocate a bridge layer.
	// This can be useful if you know that the workload this network will provide
	// services for will require attaching virtual interfaces to this interface.
	UseBridge bool `json:"use_bridge,omitempty"`
	Configure bool `json:"configure,omitempty"`
	// The name of the network.  Must be globally unique.
	Name string `json:"name,omitempty"`
	// The description of this network.
	Description string `json:"description,omitempty"`
	// The IPv6 prefix this network should use to hand out IPv6 addresses.
	// If empty, no IPv6 addresses will be allocated.  If the address is not in CIDR
	// form, Crowbar will assume it is a /64 subnet.
	V6Prefix string `json:"v6prefix,omitempty"`
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
	Conduit  string `json:"conduit,omitempty"`
	Category string `json:"category,omitempty"`
	Group    string `json:"group,omitempty"`
	lastJson []byte
}

func (o *Network) Id() string {
	if o.ID != 0 {
		return strconv.FormatInt(o.ID, 10)
	} else if o.Name != "" {
		return o.Name
	} else {
		log.Panic("Network has no ID or name")
		return ""
	}
}

func (o *Network) SetId(s string) error {
	if o.ID != 0 || o.Name != "" {
		return errors.New("SetId can only be used on an un-IDed object")
	}
	if id, err := strconv.ParseInt(s, 10, 64); err == nil {
		o.ID = id
	} else {
		o.Name = s
	}
	return nil
}

func (o *Network) ApiName() string {
	return "networks"
}

func (o *Network) Match() (res []*Network, err error) {
	res = make([]*Network, 0)
	return res, session.match(o, &res, o.ApiName(), "match")
}

func (o *Network) setLastJSON(b []byte) {
	o.lastJson = make([]byte, len(b))
	copy(o.lastJson, b)
}

func (o *Network) lastJSON() []byte {
	return o.lastJson
}

// Role gets the Crowbar Role that is responsible for configuring the
// Network on a given Node.
func (o *Network) Role() (role *Role, err error) {
	role = &Role{Name: fmt.Sprintf("network-%v", o.Name)}
	return role, Read(role)
}

// Satisfy salient interfaces
func (o *Network) networkRanges()      {}
func (o *Network) networkAllocations() {}
func (o *Network) networkRouters()     {}

type Networker interface {
	Crudder
	networks()
}

// Networks returns all of the Networks.
func Networks(scope ...Networker) (res []*Network, err error) {
	paths := make([]string, len(scope))
	for i := range scope {
		paths[i] = url(scope[i])
	}

	res = make([]*Network, 0)
	return res, session.list(&res, append(paths, "networks")...)
}

// NetworkRange defines a specific range of allocatable addresses in a
// Network.  All address allocation for a Node must come from one
// specific Range per Network.  NetworkRanges are allowed to have
// different teaming, VLAN, bonding, and conduit specifications than
// their parent Network.
type NetworkRange struct {
	ID          int64  `json:"id,omitempty"`
	NetworkID   int64  `json:"network_id,omitempty"`
	Vlan        int64  `json:"vlan,omitempty"`
	TeamingMode int64  `json:"team_mode,omitempty"`
	UseTeam     bool   `json:"use_team,omitempty"`
	UseVlan     bool   `json:"use_vlan,omitempty"`
	UseBridge   bool   `json:"use_bridge,omitempty"`
	Overlap     bool   `json:"overlap,omitempty"`
	Conduit     string `json:"conduit,omitempty"`
	// The first address that can be allocated in this NetworkRange.
	// The address can be either IPv4 or IPv6, and must be in CIDR form.
	First string `json:"first,omitempty"`
	// The last address that can be callocated from this NetworkRange.
	// It must be of the same type and in the same subnet as the First address.
	Last     string `json:"last,omitempty"`
	Name     string `json:"string,omitempty"`
	lastJson []byte
}

func (o *NetworkRange) Id() string {
	if o.ID != 0 {
		return strconv.FormatInt(o.ID, 10)
	} else if o.Name != "" {
		return o.Name
	} else {
		log.Panic("NetworkRange has no ID or name")
		return ""
	}
}

func (o *NetworkRange) SetId(s string) error {
	if o.ID != 0 {
		return errors.New("SetId can only be used on an un-IDed object")
	}
	if id, err := strconv.ParseInt(s, 10, 64); err == nil {
		o.ID = id
	} else {
		return err
	}
	return nil
}

func (o *NetworkRange) ApiName() string {
	return "network_ranges"
}

func (o *NetworkRange) Match() (res []*NetworkRange, err error) {
	res = make([]*NetworkRange, 0)
	return res, session.match(o, &res, o.ApiName(), "match")
}

func (o *NetworkRange) setLastJSON(b []byte) {
	o.lastJson = make([]byte, len(b))
	copy(o.lastJson, b)
}

func (o *NetworkRange) lastJSON() []byte {
	return o.lastJson
}

func (o *NetworkRange) Network() (*Network, error) {
	res := &Network{ID: o.NetworkID}
	return res, Read(res)
}

func (o *NetworkRange) networkAllocations() {}

type NetworkRanger interface {
	Crudder
	networkRanges()
}

// Networks returns all of the Networks.
func NetworkRanges(scope ...NetworkRanger) (res []*NetworkRange, err error) {
	paths := make([]string, len(scope))
	for i := range scope {
		paths[i] = url(scope[i])
	}

	res = make([]*NetworkRange, 0)
	return res, session.list(&res, append(paths, "network_ranges")...)
}

// NetworkAllocation is the allocation of an address from a NetworkRange to a Node.
type NetworkAllocation struct {
	ID             int64 `json:"id,omitempty"`
	NodeID         int64 `json:"node_id,omitempty"`
	NetworkID      int64 `json:"network_id,omitempty"`
	NetworkRangeID int64 `json:"network_range_id,omitempty"`
	// Address is an IPv4 or v6 address in CIDR format.
	Address  string `json:"address,omitempty"`
	lastJson []byte
}

func (o *NetworkAllocation) Id() string {
	if o.ID != 0 {
		return strconv.FormatInt(o.ID, 10)
	} else if o.Address != "" {
		return o.Address
	}
	log.Panic("NetworkAllocation has no ID or Address")
	return ""
}

func (o *NetworkAllocation) SetId(s string) error {
	if o.ID != 0 || o.Address != "" {
		return errors.New("SetId can only be used on an un-IDed object")
	}
	if id, err := strconv.ParseInt(s, 10, 64); err == nil {
		o.ID = id
	} else {
		o.Address = s
	}
	return nil
}

func (o *NetworkAllocation) ApiName() string {
	return "network_allocations"
}

func (o *NetworkAllocation) Match() (res []*NetworkAllocation, err error) {
	res = make([]*NetworkAllocation, 0)
	return res, session.match(o, &res, o.ApiName(), "match")
}

func (o *NetworkAllocation) setLastJSON(b []byte) {
	o.lastJson = make([]byte, len(b))
	copy(o.lastJson, b)
}

func (o *NetworkAllocation) lastJSON() []byte {
	return o.lastJson
}

func (o *NetworkAllocation) Node() (*Node, error) {
	res := &Node{ID: o.NodeID}
	return res, Read(res)
}

func (o *NetworkAllocation) Network() (*Network, error) {
	res := &Network{ID: o.NetworkID}
	return res, Read(res)
}

type NetworkAllocater interface {
	Crudder
	networkAllocations()
}

// Networks returns all of the Networks.
func NetworkAllocations(scope ...NetworkAllocater) (res []*NetworkAllocation, err error) {
	paths := make([]string, len(scope))
	for i := range scope {
		paths[i] = url(scope[i])
	}

	res = make([]*NetworkAllocation, 0)
	return res, session.list(&res, append(paths, "network_allocations")...)
}

type NetworkRouter struct {
	ID        int64  `json:"id,omitempty"`
	NetworkID int64  `json:"network_id,omitempty"`
	Address   string `json:"address,omitempty"`
	Pref      int64  `json:"pref,omitempty"`
	CreatedAt string `json:"created_at,omitempty"`
	UpdatedAt string `json:"updated_at,omitempty"`
	lastJson  []byte
}

func (o *NetworkRouter) Id() string {
	if o.ID != 0 {
		return strconv.FormatInt(o.ID, 10)
	} else if o.Address != "" {
		return o.Address
	}
	log.Panic("NetworkAllocation has no ID or Address")
	return ""
}

func (o *NetworkRouter) SetId(s string) error {
	if o.ID != 0 || o.Address != "" {
		return errors.New("SetId can only be used on an un-IDed object")
	}
	if id, err := strconv.ParseInt(s, 10, 64); err == nil {
		o.ID = id
	} else {
		o.Address = s
	}
	return nil
}

func (o *NetworkRouter) ApiName() string {
	return "network_routers"
}

func (o *NetworkRouter) Match() (res []*NetworkRouter, err error) {
	res = make([]*NetworkRouter, 0)
	return res, session.match(o, &res, o.ApiName(), "match")
}

func (o *NetworkRouter) setLastJSON(b []byte) {
	o.lastJson = make([]byte, len(b))
	copy(o.lastJson, b)
}

func (o *NetworkRouter) lastJSON() []byte {
	return o.lastJson
}

func (o *NetworkRouter) Network() (*Network, error) {
	res := &Network{ID: o.NetworkID}
	return res, Read(res)
}

type NetworkRouterer interface {
	Crudder
	networkRouters()
}

// Networks returns all of the Networks.
func NetworkRouters(scope ...NetworkRouterer) (res []*NetworkRouter, err error) {
	paths := make([]string, len(scope))
	for i := range scope {
		paths[i] = url(scope[i])
	}

	res = make([]*NetworkRouter, 0)
	return res, session.list(&res, append(paths, "network_routers")...)
}
