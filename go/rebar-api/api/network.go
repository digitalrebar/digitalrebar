package api

import (
	"errors"
	"fmt"
	"net/url"
	"path"

	"github.com/digitalrebar/digitalrebar/go/rebar-api/datatypes"
)

// Network wraps datatypes.Network to provide client API functionality
type Network struct {
	datatypes.Network
	Timestamps
	apiHelper
	rebarSrc
}

// Role gets the Rebar Role that is responsible for configuring the
// Network on a given Node.
func (o *Network) Role() (role *Role, err error) {
	role = &Role{}
	role.Name = fmt.Sprintf("network-%v", o.Name)
	return role, o.client().Read(role)
}

// AutoRanges returns the NetworkRanges in a Network for a specific Node.
func (o *Network) AutoRanges(node *Node) ([]*NetworkRange, error) {
	netId, err := o.Id()
	if err != nil {
		return nil, err
	}
	nodeId, err := node.Id()
	if err != nil {
		return nil, err
	}
	uri := path.Join(o.ApiName(), netId, "auto_ranges", nodeId)
	res := make([]*NetworkRange, 0)
	return res, o.client().List(uri, &res)
}

// ForAddress allows you to look up the network that contains this address.
func (o *Network) ForAddress(addr string) error {
	p := fmt.Sprintf("%v?address=%v", o.ApiName(), url.QueryEscape(addr))
	buf, err := o.client().request("GET", p, nil)
	if err != nil {
		return err
	}
	return o.client().unmarshal(p, buf, o)
}

// Satisfy salient interfaces
func (o *Network) networkRanges()      {}
func (o *Network) networkAllocations() {}
func (o *Network) networkRouters()     {}

// Networker is anything that a Network can be added or removed from.
type Networker interface {
	Crudder
	networks()
}

// Networks returns all of the Networks.
func (c *Client) Networks(scope ...Networker) (res []*Network, err error) {
	paths := make([]string, len(scope))
	for i := range scope {
		paths[i] = fragTo(scope[i])
	}
	n := &Network{}
	paths = append(paths, n.ApiName())
	res = make([]*Network, 0)
	return res, c.List(c.UrlFor(n, paths...), &res)
}

// NetworkRange wraps datatypes.NetworkRange to provide the client API
type NetworkRange struct {
	datatypes.NetworkRange
	Timestamps
	apiHelper
	rebarSrc
}

// Network returns the Network that owns this NetworkRange
func (o *NetworkRange) Network() (*Network, error) {
	res := &Network{}
	res.ID = o.NetworkID
	return res, o.client().Read(res)
}

func (o *NetworkRange) networkAllocations() {}

// NetworkRanger is anything that a NetworkRange can be bound to.
type NetworkRanger interface {
	Crudder
	networkRanges()
}

// NetworkRanges returns all of the NetworkRanges
func (c *Client) NetworkRanges(scope ...NetworkRanger) (res []*NetworkRange, err error) {
	paths := make([]string, len(scope))
	for i := range scope {
		paths[i] = fragTo(scope[i])
	}
	nr := &NetworkRange{}
	paths = append(paths, nr.ApiName())
	res = make([]*NetworkRange, 0)
	return res, c.List(c.UrlFor(nr, paths...), &res)
}

// NetworkAllocation wraps datatypes.NetworkAllocation to provide the client API.
type NetworkAllocation struct {
	datatypes.NetworkAllocation
	Timestamps
	apiHelper
	rebarSrc
}

// Node returns the Node that this NetowrkAllocation is bound to, if any.
func (o *NetworkAllocation) Node() (*Node, error) {
	res := &Node{}
	if o.NodeID.Valid {
		res.ID = o.NodeID.Int64
		return res, o.client().Read(res)
	}
	return nil, errors.New("NetworkAllocation not bound to a Node")
}

// Network returns the Network this Allocation belongs to.
func (o *NetworkAllocation) Network() (*Network, error) {
	res := &Network{}
	if o.NetworkID.Valid {
		res.ID = o.NetworkID.Int64
		return res, o.client().Read(res)
	}
	return nil, errors.New("NetworkAllocation not bound to a Network")
}

// NetworkAllocator is anything that a NetworkAllocation can be bound to.
type NetworkAllocater interface {
	Crudder
	networkAllocations()
}

// NetworkAllocations returns all of the NetworkAllocations.
func (c *Client) NetworkAllocations(scope ...NetworkAllocater) (res []*NetworkAllocation, err error) {
	paths := make([]string, len(scope))
	for i := range scope {
		paths[i] = fragTo(scope[i])
	}
	na := &NetworkAllocation{}
	paths = append(paths, na.ApiName())
	res = make([]*NetworkAllocation, 0)
	return res, c.List(c.UrlFor(na, paths...), &res)
}

// NetworkRouter wraps datatypes.NetworkRouter to provide the client API.
type NetworkRouter struct {
	datatypes.NetworkRouter
	Timestamps
	apiHelper
	rebarSrc
}

// Network returns the Network that this NetworkRouter belongs to.
func (o *NetworkRouter) Network() (*Network, error) {
	res := &Network{}
	res.ID = o.NetworkID
	return res, o.client().Read(res)
}

// NetworkRouterer is anything that a NetworkRouter can be bound to.
type NetworkRouterer interface {
	Crudder
	networkRouters()
}

// Networks returns all of the Networks.
func (c *Client) NetworkRouters(scope ...NetworkRouterer) (res []*NetworkRouter, err error) {
	paths := make([]string, len(scope))
	for i := range scope {
		paths[i] = fragTo(scope[i])
	}
	nr := &NetworkRouter{}
	paths = append(paths, nr.ApiName())
	res = make([]*NetworkRouter, 0)
	return res, c.List(c.UrlFor(nr, paths...), &res)
}
