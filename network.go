package crowbar

import (
	"errors"
	"fmt"
	"path"

	"github.com/VictorLowther/crowbar-api/datatypes"
)

type Network struct {
	datatypes.Network
	Timestamps
	apiHelper
}

// Role gets the Crowbar Role that is responsible for configuring the
// Network on a given Node.
func (o *Network) Role() (role *Role, err error) {
	role = &Role{}
	role.Name = fmt.Sprintf("network-%v", o.Name)
	return role, Read(role)
}

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
	return res, List(uri, &res)
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
	paths = append(paths, "networks")
	res = make([]*Network, 0)
	return res, List(path.Join(paths...), &res)
}

type NetworkRange struct {
	datatypes.NetworkRange
	Timestamps
	apiHelper
}

func (o *NetworkRange) Network() (*Network, error) {
	res := &Network{}
	res.ID = o.NetworkID
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
	paths = append(paths, "network_ranges")
	res = make([]*NetworkRange, 0)
	return res, List(path.Join(paths...), &res)
}

type NetworkAllocation struct {
	datatypes.NetworkAllocation
	Timestamps
	apiHelper
}

func (o *NetworkAllocation) Node() (*Node, error) {
	res := &Node{}
	if o.NodeID.Valid {
		res.ID = o.NodeID.Int64
		return res, Read(res)
	}
	return nil, errors.New("NetworkAllocation not bound to a Node")
}

func (o *NetworkAllocation) Network() (*Network, error) {
	res := &Network{}
	if o.NetworkID.Valid {
		res.ID = o.NetworkID.Int64
		return res, Read(res)
	}
	return nil, errors.New("NetworkAllocation not bound to a Network")
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
	paths = append(paths, "network_allocations")
	res = make([]*NetworkAllocation, 0)
	return res, List(path.Join(paths...), &res)
}

type NetworkRouter struct {
	datatypes.NetworkRouter
	Timestamps
	apiHelper
}

func (o *NetworkRouter) Network() (*Network, error) {
	res := &Network{}
	res.ID = o.NetworkID
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
	paths = append(paths, "network_routers")
	res = make([]*NetworkRouter, 0)
	return res, List(path.Join(paths...), &res)
}
